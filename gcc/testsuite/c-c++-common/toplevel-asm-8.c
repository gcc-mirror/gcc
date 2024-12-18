/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void
foo (void)
{
  int m;
  asm ("" : "=-m" (m));		/* { dg-error "'-' modifier used inside of a function" } */
  asm ("" : : "-m" (m));	/* { dg-error "'-' modifier used inside of a function" } */
  asm ("" : : "-i" (32));	/* { dg-error "'-' modifier used inside of a function" } */
  asm ("" : : "-s" (foo));	/* { dg-error "'-' modifier used inside of a function" } */
}
