/* { dg-do compile } */

unsigned a;
unsigned *b;
void fn1() {
    char c;
    while (b < (unsigned *)fn1)
      {
	a += b[2] + c;
	b++;
      }
}
