/* { dg-require-weak "" } */
/* { dg-skip-if "No undefined weak" { hppa*-*-hpux* && { ! lp64 } } } */
/* { dg-skip-if "No undefined weak" { nvptx-*-* } } */
/* { dg-additional-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } } */
/* { dg-additional-options "-Wl,-flat_namespace" { target *-*-darwin[89]* } } */
/* { dg-additional-options "--param vect-epilogues-nomask=1 -mavx2" { target avx2_runtime } } */

#define SIZE 1023
#define ALIGN 64

extern int posix_memalign(void **memptr, __SIZE_TYPE__ alignment, __SIZE_TYPE__ size) __attribute__((weak));
extern void free (void *);

void __attribute__((noinline))
test_citer (int * __restrict__ a,
	    int * __restrict__ b,
	    int * __restrict__ c)
{
  int i;

  a = (int *)__builtin_assume_aligned (a, ALIGN);
  b = (int *)__builtin_assume_aligned (b, ALIGN);
  c = (int *)__builtin_assume_aligned (c, ALIGN);

  for (i = 0; i < SIZE; i++)
    c[i] = a[i] + b[i];
}

void __attribute__((noinline))
test_viter (int * __restrict__ a,
	    int * __restrict__ b,
	    int * __restrict__ c,
	    int size)
{
  int i;

  a = (int *)__builtin_assume_aligned (a, ALIGN);
  b = (int *)__builtin_assume_aligned (b, ALIGN);
  c = (int *)__builtin_assume_aligned (c, ALIGN);

  for (i = 0; i < size; i++)
    c[i] = a[i] + b[i];
}

void __attribute__((noinline))
init_data (int * __restrict__ a,
	   int * __restrict__ b,
	   int * __restrict__ c,
	   int size)
{
  for (int i = 0; i < size; i++)
    {
      a[i] = i;
      b[i] = -i;
      c[i] = 0;
      asm volatile("": : :"memory");
    }
  a[size] = b[size] = c[size] = size;
}


void __attribute__((noinline))
run_test ()
{
  int *a;
  int *b;
  int *c;
  int i;

  if (posix_memalign ((void **)&a, ALIGN, (SIZE + 1) * sizeof (int)) != 0)
    return;
  if (posix_memalign ((void **)&b, ALIGN, (SIZE + 1) * sizeof (int)) != 0)
    return;
  if (posix_memalign ((void **)&c, ALIGN, (SIZE + 1) * sizeof (int)) != 0)
    return;

  init_data (a, b, c, SIZE);
  test_citer (a, b, c);
  for (i = 0; i < SIZE; i++)
    if (c[i] != a[i] + b[i])
      __builtin_abort ();
  if (a[SIZE] != SIZE || b[SIZE] != SIZE || c[SIZE] != SIZE)
    __builtin_abort ();

  init_data (a, b, c, SIZE);
  test_viter (a, b, c, SIZE);
  for (i = 0; i < SIZE; i++)
    if (c[i] != a[i] + b[i])
      __builtin_abort ();
  if (a[SIZE] != SIZE || b[SIZE] != SIZE || c[SIZE] != SIZE)
    __builtin_abort ();

  free (a);
  free (b);
  free (c);
}

int
main (int argc, const char **argv)
{
  if (!posix_memalign)
    return 0;

  run_test ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" { target avx2_runtime } } } */
/* { dg-final { scan-tree-dump-times "LOOP EPILOGUE VECTORIZED \\(MODE=V16QI\\)" 2 "vect" { target avx2_runtime } } } */
