/* { dg-do assemble } */
/* { dg-options "-Os -fno-omit-frame-pointer" } */

void f (float, float, float, float, float, float, float, float, float, float);

void g (void)
{
  f (0, 0, 0, 0, 0, 0, 0, 0, -1, 1);
}
