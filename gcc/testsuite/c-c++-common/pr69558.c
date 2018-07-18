/* PR c/69558 */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations" } */

/* TODO: XFAIL for g++ (works for C).  */

#define A \
  _Pragma ("GCC diagnostic push") \
  _Pragma ("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
#define B \
  _Pragma ("GCC diagnostic pop")
#define C(x) \
  A \
  static inline void bar (void) { x (); } /* { dg-bogus "in definition of|deprecated" "" { xfail { c++ } } } */ \
  B

__attribute__((deprecated)) void foo (void); /* { dg-bogus "declared here" "" { xfail { c++ } } } */

C (foo) /* { dg-bogus "is deprecated" } */
