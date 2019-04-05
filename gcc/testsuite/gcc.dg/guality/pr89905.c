/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
char c, d = 22, f;
short e, g;
int h;
char(a)() {}
char(b)() { return 0; }
void i() {
    char j;
    for (; h < 1;) {
	short k[9] = {1, 1, 1, 1, 1, 1, 1, 1, 1};
	int l, i = 5;
	short m[3] = {0, 0, 0};
	for (; h < 7; h++)
	  for (; d >= 33;) {
	      ++k[8];
	      f = (c || a()) && g;
	  }
	i++;
	j = b() || m[2];
	l = 0;
	for (; l <= 6; l = d)
	  e = k[8];
	/* i may very well be optimized out, so we cannot test for i == 6.
	   Instead test i + 1 which will make the test UNSUPPORTED if i
	   is optimized out.  Since the test previously had wrong debug
	   with i == 5 this is acceptable.  Optimally we'd produce a
	   debug stmt for the final value of the loop which would fix
	   the UNSUPPORTED cases.  */
	optimize_me_not(); /* { dg-final { gdb-test . "i + 1" "7" } } */
    }
}
int main() { i(); }
