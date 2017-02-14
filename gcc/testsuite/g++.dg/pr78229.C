/* { dg-do compile { target x86_64-*-* i?86-*-* } } */
/* { dg-options "-O2 -mbmi -w" } */

void a();
inline int b(int c) {
    int d = c;
    return __builtin_ia32_tzcnt_u32(d);
}
struct e {};
int f, g, h;
void fn3() {
    float j;
    &j;
      {
	e k;
	while (h) {
	    if (g == 0)
	      continue;
	    int i = b(g);
	    f = i;
	}
	a();
      }
}
