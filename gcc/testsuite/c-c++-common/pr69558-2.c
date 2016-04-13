/* PR c/69558 */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations" } */

/* Verify disabling -Wdeprecated-declarations, where neither the _Pragma nor
   the affected code are in macros.  */

__attribute__((deprecated)) void foo (void);

void bar (void)
{
  _Pragma ("GCC diagnostic push")
  _Pragma ("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
  foo ();
  _Pragma ("GCC diagnostic pop")
}
