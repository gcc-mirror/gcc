/* { dg-do compile } */
/* { dg-options "-Os -fsched2-use-superblocks -g" } */

void x (int a)
{
  __label__ xlab;
  void y (int b)
  {
    switch (b)
      {
      case 1:
	goto xlab;
      case 2:
	goto xlab;
      }
  }
  y (a);
xlab:;
}
