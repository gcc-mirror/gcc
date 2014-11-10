/* Test TREE_CONSTANT VLA size: bug 27893.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-require-effective-target alloca } */
void g(void *);
void f(void) { int b[1/0]; g(b); }
