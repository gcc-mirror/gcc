/* PR c/14114 */
/* Origin: <snyder@fnal.gov> */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

/* This used to fail because the compiler thought that the
   declaration of 'c' from 'b' was shadowing that from 'a'.  */

void a()
{
  void c();
  c();
}

void b()
{
  void c();
}

void c() {}
