/* { dg-do compile } */
/* { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */
__attribute__ ((vector_size (64))) unsigned char v1, v2, v3;
void
vadd (void)
{
  v1 = v2 + v3;
}
void
test_add (void)
{
  vadd ();
}
void
vsub (void)
{
  v1 = v2 - v3;
}
