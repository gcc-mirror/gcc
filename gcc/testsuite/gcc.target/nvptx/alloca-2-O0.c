/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alloca_ptx } } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */

int
main(void)
{
  return !(__builtin_alloca(100) != __builtin_alloca(10));
}
/* { dg-final { scan-assembler-times {(?n)\talloca\.u64\t%r[0-9]+_local, %r[0-9]+;$} 2 } } */
