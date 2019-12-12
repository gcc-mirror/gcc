/* PR tree-optimization/87034 - missing -Wformat-overflow on a sprintf
   %s with a wide string
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow -ftrack-macro-expansion=0" } */

typedef __WCHAR_TYPE__ wchar_t;

extern int sprintf (char*, const char*, ...);

struct S
{
  char a[1];
  void (*pf)(void);
};

void test (struct S *p)
{
  const char *q = sizeof (wchar_t) == 2
    ? (char*)L"\x4142\x4344" : (char*)L"\x41424344\x45464748";

  sprintf (p->a, "%s", q);   /* { dg-warning "\\\[-Wformat-overflow" "pr87034" { xfail *-*-*} } */
}
