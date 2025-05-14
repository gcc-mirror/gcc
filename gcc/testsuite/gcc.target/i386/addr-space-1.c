/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "movl\[ \t\]%gs:\\((%eax|%rax)\\), %eax" } } */

extern __seg_gs int *call_me (void);

int
read_seg_gs (void)
{
  return *call_me();
}
