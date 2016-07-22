/* PR c++/66561 - __builtin_LINE at al. should yield constant expressions */
/* { dg-do compile } */

#if __cplusplus >= 201103L
#  define Assert(expr) static_assert ((expr), #expr)
#elif __STDC_VERSION__ >= 201112L
#  define Assert(expr) _Static_assert ((expr), #expr)
#else
#  define CONCAT(a, b)  a ## b
#  define CAT(a, b)     CONCAT (a, b)
#  define Assert(expr)  typedef int CAT (Assert_, __LINE__) [1 - 2 * !(expr)]
#endif

/* Verify (in C) that __builtin_FILE() yields an address constant.
   This test is ineffective in C++ where initializers of global
   objects need not be constant expressions.  */
const char* const file = __builtin_FILE ();

/* Verify (in C) that __builtin_FUNCTION() yields an address constant.  */
const char* const function = __builtin_FUNCTION ();

/* Also verify that __builtin_constant_p() returns true for both.  */
Assert (__builtin_constant_p (__builtin_FILE ()));
Assert (__builtin_constant_p (__builtin_FUNCTION ()));
	
/* Verify (in both C and C++ 11 and later) that both __builtin_FILE ()
   and __builtin_FUNCTION() yield an address constant by making use
   of a GCC extension that allows operands of arithmetic constant
   expressions to be address constants.  (Subtracting two literals
   from one another is undefined in both C and C++ and should be
   diagnosed.  See c/70772.)  */

#pragma GCC diagnostic push  
#pragma GCC diagnostic ignored "-Waddress"

enum E0 {
  e0 = __FILE__ - __FILE__,
  e1 = __builtin_FILE () - __builtin_FILE (),

#if !__cplusplus || __cplusplus >= 201103L
  /* Skip this test in C++ 98 where GCC rejects __FUNCTION__ in constant
     expressions.  */
  e2 = __FUNCTION__ - __FUNCTION__,
  e3 = __builtin_FUNCTION () - __builtin_FUNCTION ()

#endif
};

#pragma GCC diagnostic pop

/* Verify that __builtin_LINE () yields an integer constant expression.  */
#line 13
int a [__builtin_LINE ()][__builtin_LINE ()];
enum F { f0 = __builtin_LINE () };
struct S { unsigned bitfield: __builtin_LINE (); } s;

Assert (__builtin_constant_p (__builtin_LINE ()));
