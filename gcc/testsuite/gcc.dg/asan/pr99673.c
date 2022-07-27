/* { dg-do compile } */
/* Skip XPASS cases.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2 -flto" } { "" } } */
/* { dg-additional-options "-Wstringop-overread" } */

struct B {
  int i;
  struct A {
    short sa[8];
  } a[2];
};

struct C {
  char n, ax[];
};

struct D { int i, j, k; };

int f (const short[8]);

void g (struct C *pc, struct D *pd, int i)
{
  struct B *pb = (void *)pc->ax;
  pd->i = pb->i;

  const short *psa = pb->a[i].sa;
  if (f (psa)) /* { dg-bogus "from a region of size" "pr99673" { xfail *-*-* } } */
    return;
}
