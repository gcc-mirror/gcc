/* { dg-do compile } */

struct S3
{
  int f3;
  int f5;
  char f6;
  int f7;
} b;
int a;
static struct S3 *c = &b;
int *d;
int main()
{
  int i;
  for (;;) {
      a = 0;
      int **e = &d;
      i = 0;
      for (; i < 2; i++)
	d = &(*c).f5;
      *e = d;
      **e = 3;
  }
  return 0;
}
