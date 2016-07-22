/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

typedef int T __attribute__((may_alias));

extern T *p;
extern int *p;

extern int *p2;
extern T *p2;

void fn1 (T);
void fn1 (int);

void fn2 (int);
void fn2 (T);

/* Ensure that the composite types have may_alias.  */
void
f (long *i)
{
  *i = *(__typeof (*p) *) &p;
  asm ("" : : "r" (*p));
  *i = *(__typeof (*p2) *) &p2;
  asm ("" : : "r" (*p2));
}
