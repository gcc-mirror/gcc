/* Qualifiers lost when taking the address of a const restrict object.
   PR 44322.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */
void * restrict const a[2];
void * restrict const (*p2)[2];

void foo(void) {
   p2 = &a;
}

void * restrict volatile b[2];
void * restrict volatile (*q2)[2];

void bar(void) {
   q2 = &b;
}
