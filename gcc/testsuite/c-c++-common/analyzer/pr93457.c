/* { dg-do compile } */

void
p5 (const void *);

void
s5 (const void *cl)
{
  p5 (&cl[1]); /* { dg-warning "dereferencing 'void \\*' pointer" "" { target c } } */
  /* { dg-warning "pointer of type 'void \\*' used in arithmetic" "" { target c++ } .-1 } */
  /* { dg-error "'const void\\*' is not a pointer-to-object type" "" { target c++ } .-2 } */
}
