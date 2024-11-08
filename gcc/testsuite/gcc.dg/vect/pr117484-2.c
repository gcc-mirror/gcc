/* { dg-do compile } */

int a;
extern int d[];
extern int b[];
extern _Bool c[];
extern char h[];
int main()
{
  for (int i = 0; i < 1024; i += 4)
    if (h[i] || c[i])
      {
	a = d[i];
	b[i] = d[i - 3];
      }
}
