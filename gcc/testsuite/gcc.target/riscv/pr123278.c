/* { dg-do compile } */
/* { dg-options "-mcpu=xt-c910v2 -mtune=andes-n45 -O2" } */
_Float16 f;

void
foo ()
{
  f *= 10;
}
