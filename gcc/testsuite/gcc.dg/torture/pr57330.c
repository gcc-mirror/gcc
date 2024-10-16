/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

void foo (int a)
{}

void *a;
void bar ()
{
  void **( *b ) (  ) = (void**(*)()) foo;
  a = b (0);
}
