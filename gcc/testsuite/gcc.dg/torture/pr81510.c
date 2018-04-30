/* { dg-do compile } */
/* { dg-additional-options "-w" } */

typedef int d;
typedef int f;
typedef long h;
int a;
int b;
int c;
int e()
{
  f *g;
  h i;
  for (;;)
    if (g)
      for (; b; b++) {
	  g = c;
	  if (a &= c) {
	      d *j = &b;
	      h k;
	      for (; i; i++) {
		  *g ?: (*j = k);
		  g = &a;
	      }
	      for (; i <= 3; i++)
		;
	  }
      }
}
