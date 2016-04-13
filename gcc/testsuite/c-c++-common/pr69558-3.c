/* PR c/69558 */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations" } */

/* Verify disabling -Wdeprecated-declarations, where the _Pragma is not
   in a macro, but the affected code *is*.  */

#define C \
  foo ();

__attribute__((deprecated)) void foo (void);

void bar (void)
{
  _Pragma ("GCC diagnostic push")
  _Pragma ("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
  C
  _Pragma ("GCC diagnostic pop")
}
