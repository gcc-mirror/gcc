/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-cp-details -fno-early-inlining" } */
/* { dg-add-options bind_pic_locally } */

struct S
{
  int a, b, c;
};

void *blah(int, void *);

#define foo_body(p)\
{ \
  int i, c = (p)->c; \
  int b = (p)->b; \
  void *v = (void *) (p); \
 \
  for (i= 0; i< c; i++) \
    v = blah(b + i, v); \
}

static void __attribute__ ((noinline))
foo_v (struct S s)
{
  foo_body (&s);
}

static void __attribute__ ((noinline))
foo_r (struct S *p)
{
  foo_body (p);
}

static void
goo_v (int a, int *p)
{
  struct S s;
  s.a = 101;
  s.b = a % 7;
  s.c = *p + 6;
  foo_v (s);
}

static void
goo_r (int a, struct S n)
{
  struct S s;
  s.a = 1;
  s.b = a + 5;
  s.c = -n.b;
  foo_r (&s);
}

void
entry ()
{
  int a;
  int v;
  struct S s;

  a = 9;
  v = 3;
  goo_v (a, &v);

  a = 100;
  s.b = 18;
  goo_r (a, s);
}

/* { dg-final { scan-ipa-dump "offset: 0, type: int, CONST: 1" "cp" } } */
/* { dg-final { scan-ipa-dump "offset: 32, type: int, PASS THROUGH: 0, op plus_expr 5" "cp" } } */
/* { dg-final { scan-ipa-dump "offset: 64, type: int, LOAD AGG: 1 \\\[offset: 32, by value], op negate_expr" "cp" } } */
/* { dg-final { scan-ipa-dump "offset: 0, type: int, CONST: 101" "cp" } } */
/* { dg-final { scan-ipa-dump "offset: 32, type: int, PASS THROUGH: 0, op trunc_mod_expr 7" "cp" } } */
/* { dg-final { scan-ipa-dump "offset: 64, type: int, LOAD AGG: 1 \\\[offset: 0, by reference], op plus_expr 6" "cp" } } */
/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[0]=1, 0\\\[32]=105, 0\\\[64]=-18" "cp" } } */
/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[0]=101, 0\\\[32]=2, 0\\\[64]=9" "cp" } } */
