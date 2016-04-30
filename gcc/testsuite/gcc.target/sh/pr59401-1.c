/* Check that the GBR address optimization does not produce wrong memory
   accesses.  In this case the GBR value must be stored to a normal register
   and a GBR memory access must not be done.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler "stc\tgbr" } } */
/* { dg-final { scan-assembler "bf|bt" } } */

typedef struct
{
  int x, y, z, w;
} tcb_t;

int
test_00 (int a, tcb_t* b)
{
  tcb_t* tcb = (a & 5) ? (tcb_t*)__builtin_thread_pointer () : b;
  return tcb->w + tcb->x;
}
