/* { dg-do compile } */

/* We used to ICE in invert_truthvalue on the void type
   2nd argument of the COND_EXPR.  */

void foo(void)
{
  int value=1;
  !(value?true:throw);
}
