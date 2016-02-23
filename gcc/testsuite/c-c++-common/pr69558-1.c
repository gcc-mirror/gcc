/* PR c/69558 */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations" } */

/* Verify disabling -Wdeprecated-declarations, where the _Pragma is in a
   macro, but the affected code is *not* in a macro.  */

#define A \
  _Pragma ("GCC diagnostic push") \
  _Pragma ("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
#define B \
  _Pragma ("GCC diagnostic pop")

__attribute__((deprecated)) void foo (void);

void bar (void)
{
  A
  foo ();
  B
}
