/* PR debug/83547 */

void
foo (void)
{
  if (({ 0; }))
    ;
  if (({ 0; 0; }))
    ;
  if (({ }))		/* { dg-error "void value not ignored as it ought to be" } */
    ;
  if (({ 0; { 0; } }))	/* { dg-error "void value not ignored as it ought to be" } */
    ;
  if (({ 0; {} }))	/* { dg-error "void value not ignored as it ought to be" } */
    ;
}
