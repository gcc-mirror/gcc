/* This used to ICE on s390x due to a bug in simplify_if_then_else.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void use (int);
void test (void)
{
  union 
   {
     unsigned long ul;
     signed char sc;
   } u;

  u.sc = 8;
  u.sc &= 25;

  use (u.sc);
}

