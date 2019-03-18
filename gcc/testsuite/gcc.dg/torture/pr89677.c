/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

int a, b, d;
unsigned c;
float e, f, g;
void h() {
    float *i = &g;
    for (; c < 10; c += 3)
      for (; d; d += 3) {
	  a = *i;
	  g = f + 0;
	  f = b + *i + (b - e + 305219) + -b + 3;
      }
}
