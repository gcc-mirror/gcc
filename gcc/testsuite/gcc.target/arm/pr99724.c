/* PR target/99724 */
/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mcpu=*" } { "-mcpu=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mabi=*" } { "-mabi=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-march=*" } { "-march=iwmmxt" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_iwmmxt_ok } */
/* { dg-options "-O1 -mcpu=iwmmxt" } */

typedef int V __attribute__((vector_size (8)));
struct __attribute__((packed)) S { char a; V b; char c[7]; };

void
foo (V *x)
{
  *x = ~*x;
}

void
bar (V *x)
{
  *x = -*x;
}

void
baz (V *x, struct S *p)
{
  V y = p->b;
  *x = y;
}
