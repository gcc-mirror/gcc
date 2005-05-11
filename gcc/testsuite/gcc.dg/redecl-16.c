/* PR 21502 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int IA[];
typedef int IA5[5];
typedef int IA10[10];
typedef IA *IAP;
typedef IA5 *IA5P;
typedef IA10 *IA10P;
extern IAP a[];
void
f (void)
{
  extern IA5P a[];
}
IAP a[] = { 0 };	/* { dg-error "previous definition" } */
extern IA10P a[];	/* { dg-error "conflicting types" } */
