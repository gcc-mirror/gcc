/* PR c/69558 */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations" } */

/* Verify disabling -Wdeprecated-declarations, where the _Pragma and the
   affected code are in different macros.  */

#define A \
  _Pragma ("GCC diagnostic push") \
  _Pragma ("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
#define B \
  _Pragma ("GCC diagnostic pop")
#define C \
  foo ();

__attribute__((deprecated)) void foo (void);

void bar (void)
{
  A
  C
  B
}
