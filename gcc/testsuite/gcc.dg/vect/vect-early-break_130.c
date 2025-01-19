/* { dg-require-effective-target mmap } */
/* { dg-add-options vect_early_break } */

#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* This was reduced from gcc/fortran/scanner.cc:gfc_widechar_to_char.
   The problem was that we omitted adding the vector skip guard when
   versioning for aliasing.  When invoked on a string that is 28 bytes
   long, that caused us to enter the vector body after having peeled 15
   iterations, leaving only 13 iterations to be performed as vector, but
   the vector body performs 16 (thus overflowing the res buffer by three
   bytes).  */
__attribute__((noipa))
void f (const uint32_t *s, char *res, int length)
{
  unsigned long i;

  for (i = 0; i < length; i++)
    {
      if (s[i] > 255)
        __builtin_abort ();
      res[i] = (char)s[i];
    }
}

int main(void)
{
  long pgsz = sysconf (_SC_PAGESIZE);
  if (pgsz == -1) {
    fprintf (stderr, "sysconf failed: %m\n");
    return 0;
  }

  void *p = mmap (NULL,
      pgsz * 2,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED) {
    fprintf (stderr, "mmap failed: %m\n");
    return 0;
  }

  if (mprotect (p + pgsz, pgsz, PROT_NONE)) {
    fprintf (stderr, "mprotect failed: %m\n");
    return 0;
  }

  uint32_t in[128];
  memset (in, 0, sizeof(in));

  uintptr_t x = (uintptr_t)in;

  /* We want to make our input pointer maximally misaligned (so we have
     to peel the greatest possible number of iterations for alignment).
     We need two bits of alignment for our uint32_t pointer to be
     aligned.  Assuming we process 16 chars per vector iteration, we
     will need to load 16 uint32_ts, thus we need a further 4 bits of
     alignment.  */
  const uintptr_t align_bits = 2 + 4;
  const uintptr_t align_p2 = (1 << align_bits);
  const uintptr_t align_p2m1 = align_p2 - 1;

  if (x & align_p2m1 <= 4)
    x &= -align_p2; /* Round down.  */
  else
    x = (x + align_p2m1) & -align_p2; /* Round up.  */

  /* Add one uint32_t to get maximally misaligned.  */
  uint32_t *inp = (uint32_t *)x + 1;

  const char *str = "dec-comparison-complex_1.f90";
  long n;
#pragma GCC novector
  for (n = 0; str[n]; n++)
    inp[n] = str[n];

  if (n > pgsz)
    __builtin_abort ();

  char *buf = p + pgsz - n;
  f (inp, buf, n);

#pragma GCC novector
  for (int i = 0; i < n; i++)
    if (buf[i] != str[i])
      __builtin_abort ();
}
