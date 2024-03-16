/* Test C23 storage class specifiers in compound literals: inline function
   constraints.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

inline void
f1 ()
{
  (static int) { 123 }; /* { dg-error "static but declared in inline function 'f1' which is not static" } */
  (static thread_local int) { 456 } ; /* { dg-error "static but declared in inline function 'f1' which is not static" } */
  (int) { 789 };
  (register int) { 1234 };
}

inline void
f1e ()
{
  (static int) { 123 };
  (static thread_local int) { 456 } ;
}

static inline void
f1s ()
{
  (static int) { 123 };
  (static thread_local int) { 456 } ;
}

inline void
f2 ()
{
  (static const int) { 123 };
  (static thread_local const int) { 456 };
}

inline void
f2e ()
{
  (static const int) { 123 };
  (static thread_local const int) { 456 };
}

static inline void
f2s ()
{
  (static const int) { 123 };
  (static thread_local const int) { 456 };
}

inline void
f3 ()
{
  (static constexpr int) { 123 };
}

inline void
f3e ()
{
  (static constexpr int) { 123 };
}

static inline void
f3s ()
{
  (static constexpr int) { 123 };
}

extern void f1e ();
extern void f2e ();
extern void f3e ();
