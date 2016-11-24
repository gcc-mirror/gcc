/* PR middle-end/78429 */
/* Testcase by Chengnian Sun <chengniansun@gmail.com> */

int a[6];
char b;
unsigned c;
short d;
volatile int e;

int foo (void)
{
  int f;
  for (; c <= 2; c++) {
    d = 3;
    for (; d >= 0; d--) {
      int g = b;
      f = a[d] || b;
    }
    f || e;
  }
  return 0;
}
