/* { dg-do compile } */

void foo (int a)
{}

void *a;
void bar ()
{
  void **( *b ) (  ) = (void**(*)()) foo;
  a = b (0);
}
