/* Check that the GBR address optimization works when there are multiple
   GBR register definitions and function calls, if the GBR is marked as a
   call saved register.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -fcall-saved-gbr" } */
/* { dg-final { scan-assembler-not "stc\tgbr" } } */

typedef struct
{
  int x, y, z, w;
} tcb_t;

extern void test_00 (void);

int
test_01 (int x, volatile int* y, int a)
{
  if (a)
    test_00 ();

  y[0] = 1;

  tcb_t* tcb = (tcb_t*)__builtin_thread_pointer ();
  return (a & 5) ? tcb->x : tcb->w;
}
