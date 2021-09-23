/* Test compatibility of unprototyped and prototyped function types (C2x makes
   the case of types affected by default argument promotions compatible).  Test
   valid-in-C2x usages.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

void f1 ();
void f1 (float);

void f2 (float);
void f2 ();

void f3 ();
void f3 (char);

void f4 (char);
void f4 ();

/* Built-in function case.  */
float sqrtf ();
