/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-fno-common" } */
/* { dg-skip-if "" { x86_64-*-mingw* } } */
/* NVPTX's weak is applied to the definition,  not declaration.  */
/* { dg-skip-if "" { nvptx-*-* } } */
/* { dg-skip-if PR119369 { amdgcn-*-* } } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?a" } } */
/* { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]_?b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?c" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?d" } } */

#pragma weak a
extern char a[];

char *user_a(void)
{
  return a+1;
}

int x;
int extern inline b(int y)
{
  return x+y;
}

extern int b(int y);

int user_b(int z)
{
  return b(z);
}

#pragma weak c
extern int c;

int *user_c = &c;

#pragma weak d
extern char d[];

char *user_d = &d[1];
