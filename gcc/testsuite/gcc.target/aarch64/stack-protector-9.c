/* { dg-options "-O2 -mcpu=neoverse-v1 -fstack-protector-all" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** main:
**	...
**	stp	x29, x30, \[sp, #?-[0-9]+\]!
**	...
**	sub	sp, sp, #[0-9]+
**	...
**	str	x[0-9]+, \[x29, #?-8\]
**	...
*/
int f(const char *);
void g(void *);
int main(int argc, char* argv[])
{
  int a;
  int b;
  char c[2+f(argv[1])];
  int d[0x100];
  char y;

  y=42; a=4; b=10;
  c[0] = 'h'; c[1] = '\0';

  c[f(argv[2])] = '\0';

  __builtin_printf("%d %d\n%s\n", a, b, c);
  g(d);

  return 0;
}
