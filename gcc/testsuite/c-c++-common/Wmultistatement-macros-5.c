/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define M(N)	\
L ## N:		\
  x++; x++ /* { dg-warning "macro expands to multiple statements" } */

int x, y, tmp;

void
fn1 (void)
{
  if (x) /* { dg-message "not guarded by this 'if' clause" } */
   M (0); /* { dg-message "in expansion of macro .M." } */
  if (x) /* { dg-message "not guarded by this 'if' clause" } */
   M (1); /* { dg-message "in expansion of macro .M." } */
}
