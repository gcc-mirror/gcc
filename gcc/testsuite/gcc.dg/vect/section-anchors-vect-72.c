/* { dg-do compile } */
/* { dg-require-effective-target section_anchors } */
/* { dg-require-effective-target vect_int } */

#define N 32

/* Clone of section-anchors-vect-70.c having nested struct.  */

struct S
{
  int e[N];
};

static struct A {
  int p1, p2;
  struct S s; 
} a, b, c;

int foo(void)
{
  for (int i = 0; i < N; i++)
    a.s.e[i] = b.s.e[i] + c.s.e[i];

   return a.s.e[0];
}

/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 0 "increase_alignment" { target aarch64*-*-* } } } */
/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 0 "increase_alignment" { target powerpc64*-*-* } } } */
/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 3 "increase_alignment" { target arm*-*-* } } } */
