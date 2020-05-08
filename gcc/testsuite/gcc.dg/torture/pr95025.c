/* { dg-do compile } */

static int a;
short b;
int *c;
void d() {
    for (;; a -= 1)
      for (; b; b += 1) {
	  *c ^= 5;
	  if (a)
	    return;
      }
}
