/* Test TREE_CONSTANT VLA size: bug 27893.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

int a;
void g(void *);
void f(void) { int b[(__SIZE_TYPE__)&a]; g(b); }
