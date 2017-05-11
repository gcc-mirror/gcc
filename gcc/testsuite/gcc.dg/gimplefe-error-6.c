/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void a(int);

__GIMPLE() void b()
{
  a_2 = 0; /* { dg-error "invalid" } */
}
