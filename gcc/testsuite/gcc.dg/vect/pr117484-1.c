/* { dg-do compile } */

extern int a;
extern short b[];
extern signed char c[], d[];
int main()
{
  for (long j = 3; j < 1024; j += 3)
    if (c[j] ? b[j] : 0) {
      b[j] = d[j - 2];
      a = d[j];
    }
}
