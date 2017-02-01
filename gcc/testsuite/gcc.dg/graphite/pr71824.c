/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int a, b, d;
int **c;
int fn1() {
    while (a)
      if (d) {
	  int e = -d;
	  for (; b < e; b++)
	    c[b] = &a;
      } else {
	  for (; b; b++)
	    c[b] = &b;
	  d = 0;
      }
}
