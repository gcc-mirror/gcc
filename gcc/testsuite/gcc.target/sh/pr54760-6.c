/* Check that the GBR address optimization works when the GBR register
   definition is not in the same basic block where the GBR memory accesses
   are.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-not "stc\tgbr" } } */

typedef struct
{
  int x, y, z, w;
} tcb_t;

int
test_00 (int a, tcb_t* b, int c)
{
  tcb_t* tcb = (tcb_t*)__builtin_thread_pointer ();
  return (a & 5) ? tcb->x : tcb->w;
}
