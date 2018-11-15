// PR66516 - missing diagnostic on taking the address of a builtin function
// { dg-do compile }

namespace std {
  // Define type_info type to be able to use typeid in tests without
  // having to include <typeinfo>.
  struct type_info {
    const char *name_;

    explicit type_info (const char *s): name_ (s) { }
    const char* name() const { return name_; }
  };
}

// Extern "C" since builtin functions used in tests have C linkage.
extern "C" {

typedef void (F)();
typedef __UINTPTR_TYPE__ uintptr_t;

// Utility function to test passing built-in functions as an ordinary
// argument and via the ellipsis.
static void func_arg (F*, ...);

// Utility function with which, along with the built-in function,
// to instantiate the C98 multi-parameter or C11 variadic tempates
// below.
void f () { }

}   // extern "C"


// Utility templates to test specializing templates on pointers and
// references to built-in functions.
template <F*> struct TestPointer { };
template <F&> struct TestReference { };

#if 201103 <= __cplusplus

template <F*...> struct TestPointers { };
template <F&...> struct TestReferences { };

#else

template <F* = &f, F* = &f> struct TestPointers { };
template <F& = f, F& = f> struct TestReferences { };

#endif

static F* test_taking_address_of_gcc_builtin ()
{
  enum UINTPTR_E { e = ~(uintptr_t)0 };

  F *p;
  void *q;
  uintptr_t a;

  __builtin_trap ();                           // okay
  (void)__builtin_trap;                        // okay
  __builtin_trap;                              // okay (if pointless)

  {
    typedef __typeof__ (__builtin_trap) F;     // okay
  }

#if 201103 <= __cplusplus
  {
    typedef decltype (__builtin_trap) F;       // okay

    a = noexcept (&__builtin_trap);
  }
#endif

  // Address and indirection operators.
  p = &__builtin_trap;                       // { dg-error "built-in" }
  p = *__builtin_trap;                       // { dg-error "built-in" }

  // Unary NOT.
  // GCC issues two diagnostics here for some reason, so account for both.
  a = !__builtin_trap;                   // { dg-error "built-in|unary" }

  // Casts.
  p = (F*)__builtin_trap;                    // { dg-error "built-in" }

  p = &(F&)__builtin_trap;                   // { dg-error "built-in" }

  p = &reinterpret_cast<F&>(__builtin_trap); // { dg-error "built-in" }
  p = &static_cast<F&>(__builtin_trap);      // { dg-error "built-in" }

  p = reinterpret_cast<F*>(__builtin_trap);  // { dg-error "built-in" }
  p = static_cast<F*>(__builtin_trap);       // { dg-error "built-in" }

  // Expect a diagnostic for an invalid static_cast of a function to
  // either uintptr_t or enum, rather than one for the argument being
  // a built-in function, since the former is more relevant than the latter.
  a = static_cast<uintptr_t>(__builtin_trap);       // { dg-error "invalid" }
  a = static_cast<UINTPTR_E>(__builtin_trap);       // { dg-error "invalid" }

  // Reinterpret cast can cast a function to uintptr_t or enum,
  // so verify that a diagnostic is issued for the use of a builtin.
  a = reinterpret_cast<uintptr_t>(__builtin_trap);  // { dg-error "built-in" }
  a = reinterpret_cast<UINTPTR_E>(__builtin_trap);  // { dg-error "built-in" }

  // Additive operator.  Ill-formed but allowed with -fpermissive.
  p = __builtin_trap + 0;            // { dg-error "built-in" }
  p = __builtin_trap - 0;            // { dg-error "built-in" }
  a = __builtin_trap - p;            // { dg-error "built-in" }
  a = p - __builtin_trap;            // { dg-error "built-in" }

  // Relational operators.  Ill-formed but allowed with -fpermissive.
  a = __builtin_trap < p;            // { dg-error "built-in|invalid template-argument-list" }
  a = p < __builtin_trap;            // { dg-error "built-in" }

  a = __builtin_trap <= p;           // { dg-error "built-in" }
  a = p <= __builtin_trap;           // { dg-error "built-in" }

  a = __builtin_trap > p;            // { dg-error "built-in" }
  a = p > __builtin_trap;            // { dg-error "built-in" }

  a = __builtin_trap > p;            // { dg-error "built-in" }
  a = p > __builtin_trap;            // { dg-error "built-in" }

  a = __builtin_trap <= p;           // { dg-error "built-in" }
  a = p <= __builtin_trap;           // { dg-error "built-in" }

  a = __builtin_trap <= p;           // { dg-error "built-in" }
  a = p <= __builtin_trap;           // { dg-error "built-in" }

  // Equality operators.
  a = __builtin_trap == p;           // { dg-error "built-in" }
  a = p == __builtin_trap;           // { dg-error "built-in" }
  a = __builtin_trap != p;           // { dg-error "built-in" }
  a = p != __builtin_trap;           // { dg-error "built-in" }

  // Logical AND and OR.
  a = __builtin_trap && p;           // { dg-error "built-in" }
  a = p && __builtin_trap;           // { dg-error "built-in" }

  a = __builtin_trap || p;           // { dg-error "built-in" }
  a = p || __builtin_trap;           // { dg-error "built-in" }

  // Conditional operator.
  a = __builtin_trap ? 1 : 0;        // { dg-error "built-in" }
  p = a ? __builtin_trap : 0;        // { dg-error "built-in" }
  p = a ? 0 : __builtin_trap;        // { dg-error "built-in" }

  // Assignment operator.
  p = __builtin_trap;                // { dg-error "built-in" }

  // Passing as an argument.
  func_arg (__builtin_trap);         // { dg-error "built-in" }
  func_arg (&__builtin_trap);        // { dg-error "built-in" }
  func_arg (*__builtin_trap);        // { dg-error "built-in" }

  // Passing through ellipsis.
  func_arg (0, __builtin_trap);      // { dg-error "built-in" }
  func_arg (0, &__builtin_trap);     // { dg-error "built-in" }
  func_arg (0, *__builtin_trap);     // { dg-error "built-in" }

  {
    // Template specialization.
    // GCC issues two diagnostics and we must account for both.
    TestPointer<__builtin_trap> tp;         // { dg-error "built-in|could not convert" }
    TestReference<__builtin_trap> tr;       // { dg-error "built-in|could not convert" }

    TestPointers<__builtin_trap> tp1;       // { dg-error "built-in|could not convert" }
    TestReferences<__builtin_trap> tr1;     // { dg-error "built-in|could not convert" }

    TestPointers<f, __builtin_trap> tp2;    // { dg-error "built-in|could not convert" }
    TestReferences<f, __builtin_trap> tr2;  // { dg-error "built-in|could not convert" }

    TestPointers<__builtin_trap, f> tp3;    // { dg-error "built-in|could not convert" }
    TestReferences<__builtin_trap, f> tr3;  // { dg-error "built-in|could not convert" }
  }

  try {
    throw __builtin_trap;                 // { dg-error "built-in" }
  }
  catch (F) { }

  return __builtin_trap;                    // { dg-error "built-in" }

  (void)a;
  (void)p;
  (void)q;
}

// Make sure operators new and delete don't trigger false positives
// (they return true from DECL_IS_BUILTIN(DECL) -- see tree.h).
void test_taking_address_of_op_new_and_delete ()
{
  typedef __SIZE_TYPE__ size_t;

  typedef void* (OpNew) (size_t);
  typedef void (OpDelete) (void*);

  OpNew &newr = operator new;
  OpNew &newra = operator new[];
  OpNew *newp = &operator new;
  newp = &operator new[];

  OpDelete &delr = operator delete;
  OpDelete &delra = operator delete[];
  OpDelete *delp = &operator delete;
  delp = &operator delete[];

  (void)newr;
  (void)newra;
  (void)newp;
  (void)delr;
  (void)delra;
  (void)delp;
}

// Helper declaration to verify that it's possible to take the address
// of a user-declared function that's also a GCC built-in.
extern int abs (int);

typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char*);

// Creating a reference to or taking the address of a built-in with
// a library "fallback" must be allowed.
void test_taking_address_of_library_builtin ()
{
  {
    typedef int F (int);

    F &r1 = __builtin_abs;
    F &r2 = *__builtin_abs;
    F *p = __builtin_abs;
    p = &__builtin_abs;
    p = *__builtin_abs;
    (void)p;
    (void)r1;
    (void)r2;
  }

  {
    typedef int F (int);

    F &r1 = abs;
    F &r2 = *abs;
    F *p = abs;
    p = &abs;
    p = *abs;
    (void)p;
    (void)r1;
    (void)r2;
  }

  {
    typedef __SIZE_TYPE__ size_t;
    typedef size_t F (const char*);
    F &r1 = __builtin_strlen;
    F &r2 = *__builtin_strlen;
    F *p = __builtin_strlen;
    p = &__builtin_strlen;
    p = *__builtin_strlen;
    (void)p;
    (void)r1;
    (void)r2;
  }

  {
    typedef size_t F (const char*);
    F &r1 = strlen;
    F &r2 = *strlen;
    F *p = strlen;
    p = &strlen;
    p = *strlen;
    (void)p;
    (void)r1;
    (void)r2;
  }
}
