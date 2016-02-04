/* PR66516 - missing diagnostic on taking the address of a builtin function
   { dg-do compile }  */

typedef void (F)(void);
typedef __UINTPTR_TYPE__ uintptr_t;

/* Utility function to test passing built-in functions as an ordinary
   argument and via the ellipsis.  */
static void func_arg (F *p, ...) { (void)p; }

static F* test_taking_address_of_gcc_builtin (void)
{
  F *p;
  void *q;
  uintptr_t a;

  /* Call, cast to void, and id are allowed.  */
  __builtin_trap ();
  (void)__builtin_trap;
  __builtin_trap;

  {
    typedef __typeof__ (__builtin_trap) F;     /* Okay.  */
  }

  /* Address and indirection operators.  */
  p = &__builtin_trap;               /* { dg-error "built-in function" }  */
  p = *__builtin_trap;               /* { dg-error "built-in function" }  */

  /* Unary NOT.  */
  a = !__builtin_trap;               /* { dg-error "built-in function" }  */

  /* Sizeof and _Alignof are disallowed by C but allowed by GCC
     and there's no reason to reject built-ins as operands since
     doing so doesn't yield their address.  */
#pragma GCC diagnostic push
  /* Disable: invalid application of 'sizeof' to a function type.  */
#pragma GCC diagnostic ignored "-Wpointer-arith"
  a = sizeof __builtin_trap;
#pragma GCC diagnostic pop

#ifndef __STDC_VERSION__
#  pragma GCC diagnostic push
  /* Disable: ISO C90 does not support '_Alignof'.  */
#  pragma GCC diagnostic ignored "-Wpedantic"
#endif

  a = _Alignof __builtin_trap;

#ifndef __STDC_VERSION__
#  pragma GCC diagnostic pop
#endif

  /* Casts.  */
  p = (F*)__builtin_trap;            /* { dg-error "built-in function" }  */
  a = (uintptr_t)__builtin_trap;     /* { dg-error "built-in function" }  */

  /* Additive operator.  */
  p = __builtin_trap + 0;            /* { dg-error "built-in function" }  */
  p = __builtin_trap - 0;            /* { dg-error "built-in function" }  */
  a = __builtin_trap - p;            /* { dg-error "built-in function" }  */
  a = p - __builtin_trap;            /* { dg-error "built-in function" }  */

  /* Relational operators.  */
  a = __builtin_trap < p;            /* { dg-error "built-in function" }  */
  a = p < __builtin_trap;            /* { dg-error "built-in function" }  */

  a = __builtin_trap <= p;           /* { dg-error "built-in function" }  */
  a = p <= __builtin_trap;           /* { dg-error "built-in function" }  */

  a = __builtin_trap > p;            /* { dg-error "built-in function" }  */
  a = p > __builtin_trap;            /* { dg-error "built-in function" }  */

  a = __builtin_trap > p;            /* { dg-error "built-in function" }  */
  a = p > __builtin_trap;            /* { dg-error "built-in function" }  */

  a = __builtin_trap <= p;           /* { dg-error "built-in function" }  */
  a = p <= __builtin_trap;           /* { dg-error "built-in function" }  */

  a = __builtin_trap <= p;           /* { dg-error "built-in function" }  */
  a = p <= __builtin_trap;           /* { dg-error "built-in function" }  */

  /* Equality operators.  */
  a = __builtin_trap == p;           /* { dg-error "built-in function" }  */
  a = p == __builtin_trap;           /* { dg-error "built-in function" }  */
  a = __builtin_trap != p;           /* { dg-error "built-in function" }  */
  a = p != __builtin_trap;           /* { dg-error "built-in function" }  */

  /* Logical AND and OR.  */
  a = __builtin_trap && p;           /* { dg-error "built-in function" }  */
  a = p && __builtin_trap;           /* { dg-error "built-in function" }  */

  a = __builtin_trap || p;           /* { dg-error "built-in function" }  */
  a = p || __builtin_trap;           /* { dg-error "built-in function" }  */

  /* Conditional operator.  */
  a = __builtin_trap ? 1 : 0;        /* { dg-error "built-in function" }  */
  p = a ? __builtin_trap : 0;        /* { dg-error "built-in function" }  */
  p = a ? 0 : __builtin_trap;        /* { dg-error "built-in function" }  */

  /* Assignment operator.  */
  p = __builtin_trap;                /* { dg-error "built-in function" }  */

  q = __builtin_trap;                /* { dg-error "built-in function" }  */
  a = __builtin_trap;                /* { dg-error "built-in function" }  */

  /* Passing as an argument.  */
  func_arg (__builtin_trap);         /* { dg-error "built-in function" }  */

  /* Passing through the ellipsis.  */
  func_arg (0, __builtin_trap);      /* { dg-error "built-in function" }  */

  /* Return statement.  */
  return __builtin_trap;             /* { dg-error "built-in function" }  */

  (void)a;
  (void)p;
  (void)q;
}

/* Helper declarations to verify that it's possible to take the address
   of a user-declared function that's also a GCC built-in.  */
extern int abs (int);

extern __SIZE_TYPE__ strlen (const char*);

/* Taking the address of a builtin with a library "fallback" must be
   allowed, either using the __builtin_xxx form or the xxx form, when
   the library fallback is declared either explicitly or implicitly
   by virtue of first calling the function.  */
void test_taking_address_of_library_builtin (int i)
{
  {
    typedef int F (int);

    /* Compute the address of libc's abs using the implicitly declared
       __builtin_abs form (all expressions are valid).  */
    F *p = __builtin_abs;
    p = &__builtin_abs;
    p = *__builtin_abs;

    /* Compute the address of libc's abs declared above.  */
    p = abs;
    p = &abs;
    p = *abs;
    (void)p;
  }

  {
    typedef __SIZE_TYPE__ size_t;
    typedef size_t F (const char*);

    /* Compute the address of libc's strlen using the implicitly
       declared __builtin_strlen form.  */
    F *p = __builtin_strlen;
    p = &__builtin_strlen;
    p = *__builtin_strlen;

    /* Compute the address of libc's strlen declared above.  */
    p = strlen;
    p = &strlen;
    p = *strlen;
    (void)p;
  }

  {
    typedef int F (int);

    /* Compute the address of libc's isxxx functions using the implicitly
       declared __builtin_xxx form.  */
    F *p = __builtin_isalnum;
    p = &__builtin_isalpha;
    p = *__builtin_iscntrl;

    /* According to C90 (see also the discussion in c/67386):
       If the expression that precedes the parenthesized argument list
       in a function call consists solely of an identifier, and if no
       declaration is visible for this identifier, the identifier is
       implicitly declared exactly as if, in the innermost block
       containing the function call, the declaration
       extern int identifier();
       appeared.  */

    /* Call the functions first to have their declarations "injected"
       into the enclosing block.  Suppress warnings.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
    i = isalnum (i) || isalpha (i) || iscntrl (i);
#pragma GCC diagnostic pop

    /* Take the address of the functions relying on their declarations
       having been implicitly provided by the calls above.  */
    p = isalnum;
    p = &isalpha;
    p = *iscntrl;
    (void)p;
  }
}
