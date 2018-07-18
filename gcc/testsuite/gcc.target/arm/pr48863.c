/* PR target/48863.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

/* Check that Temporary Expression Replacement does not move a
  libcall-producing expression across a statement initialising a local
  register variable.  */

static inline int
dosvc (int fd, unsigned long high, unsigned low)
{
  register int r0 asm("r0") = fd;
  register int r2 asm("r2") = high;
  register int r3 asm("r3") = low;

  asm volatile("" : "=r"(r0) : "0"(r0), "r"(r2), "r"(r3));
  return r0;
}

struct s
{
  int fd;
  long long length;
} s = { 2, 0 }, *p = &s;

int
main (void)
{
  unsigned low = p->length & 0xffffffff;
  unsigned high = p->length / 23;

  if (dosvc (p->fd, high, low) != 2)
    __builtin_abort ();
  return 0;
}
