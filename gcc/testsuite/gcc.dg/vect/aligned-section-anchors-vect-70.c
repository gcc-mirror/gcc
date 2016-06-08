/* { dg-do compile } */
/* { dg-require-effective-target section_anchors } */
/* { dg-require-effective-target vect_int } */

#define N 32

/* Increase alignment of struct if an array's offset is multiple of alignment of
   vector type corresponding to it's scalar type.
   For the below test-case:
   offsetof(e) == 8 bytes. 
   i) For arm: let x = alignment of vector type corresponding to int,
   x == 8 bytes.
   Since offsetof(e) % x == 0, set DECL_ALIGN(a, b, c) to x.
   ii) For aarch64, ppc: x == 16 bytes.
   Since offsetof(e) % x != 0, don't increase alignment of a, b, c.
*/

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
/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 3 "increase_alignment" { target arm*-*-* } } } */
