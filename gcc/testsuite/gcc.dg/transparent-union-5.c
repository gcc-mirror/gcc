/* PR 24255 */
/* { dg-do run } */
/* { dg-options "-O" } */

extern void abort (void);

union wait { int w_status; };

typedef union
{
  union wait *uptr;
  int *iptr;
} WAIT_STATUS __attribute__ ((__transparent_union__));

int status;
union wait wstatus;

void __attribute__((noinline))
test1 (WAIT_STATUS s)
{
  if (s.iptr != &status)
    abort ();
}

void __attribute__((noinline))
test2 (WAIT_STATUS s)
{
  if (s.uptr != &wstatus)
    abort ();
}

int main()
{
  test1 (&status);
  test2 (&wstatus);
  return 0;
}
