/* PR78413.  These previously failed in tree if-conversion due to a loop
   latch with multiple predecessors that the code did not anticipate.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fno-strict-aliasing" } */

extern long long int llrint(double x);
int a;
double b;
__attribute__((cold)) void decode_init() {
  int c, d = 0;
  for (; d < 12; d++) {
    if (d)
      b = 0;
    c = 0;
    for (; c < 6; c++)
      a = b ? llrint(b) : 0;
  }
}

struct S {
  _Bool bo;
};
int a, bb, c, d;
void fn1() {
  do
    do
      do {
	struct S *e = (struct S *)1;
	do
	  bb = a / (e->bo ? 2 : 1);
	while (bb);
      } while (0);
    while (d);
  while (c);
}
