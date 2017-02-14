/* { dg-do compile } */

int b, c;
long d, f;
void fn1()
{
  char g;
  long long h = 0;
  int *i;
  if (0) {
L2:
      b && (b = f);
      d = 3;
      for (; d;) {
	  char *j = &g;
	  c = *j = 0;
L3:
	  *j %= b;
	  for (; g <= 4;)
	    ;
      }
      goto L2;
  }
  for (; *i; *i = 1) {
      if ((h -= 4) == (h != (b ?: d))) {
	  g = 3;
	  goto L3;
      }
      i = (int *)&h;
      *i = f;
      i = (int *)&f;
      if ((h && 6) - (h = 0))
	goto L2;
  }
  for (; d;)
    goto L3;
}
