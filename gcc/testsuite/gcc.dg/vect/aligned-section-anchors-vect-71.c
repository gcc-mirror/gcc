/* { dg-do compile } */
/* { dg-require-effective-target section_anchors } */
/* { dg-require-effective-target vect_int } */

/* Should not increase alignment of the struct because
   sizeof (A.e) < sizeof(corresponding vector type).  */

#define N 3

static struct A {
  int p1, p2;
  int e[N];
} a, b, c;

int foo(void)
{
  for (int i = 0; i < N; i++)
    a.e[i] = b.e[i] + c.e[i];

   return a.e[0];
}

/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 0 "increase_alignment" { target aarch64*-*-* } } } */
/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 0 "increase_alignment" { target powerpc64*-*-* } } } */
/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 0 "increase_alignment" { target arm*-*-* } } } */
