/* Test old-style function definitions not in C2x: () does not give
   type with a prototype except for function definitions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

void f1 ();

/* Prototyped function returning a pointer to unprototyped function.  */
void (*f2 (void))() { return f1; }

void
g (void)
{
  f1 (1);
  f2 () (1);
}
