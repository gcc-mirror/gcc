/* PR tree-optimization/84238 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target alloca } */

char a[1];
int b;
char *strncpy (char *, char *, __SIZE_TYPE__);
void
c ()
{
  char d[b];
  strncpy (a, &d[3], 3); /* { dg-warning "writing 3 bytes into a region of size 1" } */
}
