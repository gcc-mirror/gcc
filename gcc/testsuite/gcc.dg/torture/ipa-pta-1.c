/* { dg-do compile { target { nonpic } } } */
/* { dg-options "-fipa-pta -fdump-ipa-pta" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

struct X { char x; char y; };

void bar (char *p);

void test1 (char a, char b, char c, char d, char e, char f, char g, char h)
{
  char *p = &a;
  p++;
  bar (p);
}

void test2 (struct X a, char b, char c, char d, char e, char f, char g, char h)
{
  char *p = &a.x;
  p++;
  bar (p);
}

void test3 (struct X a, char b, char c, char d, char e, char f, char g, char h)
{
  char *p = &a.y;
  bar (p);
}

void test4 (int a, char b, char c, char d, char e, char f, char g, char h)
{
  char *p = (char *)&a;
  p++;
  p++;
  p++;
  p++;
  bar (p);
}

/* { dg-final { scan-ipa-dump "bar.arg0 = { test4.arg0 test3.arg0 test2.arg0 test1.arg0 }" "pta" } } */
/* { dg-final { cleanup-ipa-dump "pta" } } */
