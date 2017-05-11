/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE() void a()
{
  __MEM() = 0; /* { dg-error "expected .<." } */
}
