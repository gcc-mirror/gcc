/* PR bootstrap/71071 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S { unsigned b : 1; } a;

void
foo ()
{
  if (a.b)
    ;
}
