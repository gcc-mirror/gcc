/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE() void a()
{
  *0 = 1; /* { dg-error "expected pointer" } */
}
