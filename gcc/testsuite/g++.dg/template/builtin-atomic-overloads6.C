/* Various atomic builtin errors are still emitted when in a fully specified
   builtin in a template.  */
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wno-pedantic" }
#include <type_traits>
#ifdef __ARM_FEATURE_SVE
#include <arm_sve.h>
#endif

/*
   Covering all if clauses, *not* all possible errors.
   E.g. load, store, exchange, compare_exchange all go through
   get_atomic_generic_size.  I ensure I test all if clauses in that function
   but do not ensure each clause is hit when using each of the different
   builtins.

   This is the stuff that is not handled by
   builtin-atomic-overloads{1,2,3,4,5}.C  */ 

class X{};
/* Want a zero-sized type in order to trigger one of the error messages.
   Don't want the error message about creating a zero sized type.
   However, *do* want to see any pedantic error messages coming from the rest
   of the testcase.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
class Zero {
    unsigned int trailing[0];
};
#pragma GCC diagnostic pop
class Large { public: int arr[10]; };
class Incomplete;
/* If there are other non-constant size types that I can use in a template
   would appreciate hearing about it (especially if they work on all targets).
   AFAIK VLA's are the other canonical example and have not managed to trigger
   the same error with those due to scoping limitations.  */
#ifdef __ARM_FEATURE_SVE
    typedef __SVUint32_t NonConstant;
#else
class NonConstant { };
#endif
typedef __UINT64_TYPE__ uint64_t;
typedef __UINT32_TYPE__ uint32_t;
// typedef _BitInt(12) Bitint;


#define INCORRECT_NUMBER_ARGUMENTS(X) \
  X(load, (int(), int(), int(), int()), 0)

#define NONPOINTER_FIRST_ARG(X) \
  X(load, (int(), int(), int()), 1)
#define INCOMPLETE_FIRST_ARG(X) \
  X(load, (Incomplete(), int(), int()), 2)
/* This won't trigger relevant fail when not using nonconstant sized type.  */
#define NONCONST_SIZE_FIRST_ARG(X) \
  X(load, (std::declval<NonConstant*>(), std::declval<NonConstant*>(), int()), 3)
#define ZEROSIZE_FIRST_ARG(X) \
  X(load, (std::declval<Zero*>(), std::declval<Zero*>(), int()), 4)

// Errors triggered by a bad type in the first position not yet triggered by
// builtin-atomic-overloads5.C.
// These are already checked to *not* give an error in the SFINAE context by
// builtin-atomic-overloads1.C.
#define FIRST_ARGS_BADTYPE(X) \
  ZEROSIZE_FIRST_ARG(X) \
  NONCONST_SIZE_FIRST_ARG(X) \
  INCOMPLETE_FIRST_ARG(X) \
  NONPOINTER_FIRST_ARG(X)

#define NONPOINTER_OTHER_ARG(X) \
  X(load, (std::declval<int*>(), int(), int()), 5)
/* This won't trigger relevant fail when not using nonconstant sized type.  */
#define NONCONST_SIZE_OTHER_ARG(X) \
  X(load, (std::declval<int*>(), std::declval<NonConstant*>(), int()), 6)
#define FUNCTIONPTR_OTHER_ARG(X) \
  X(load, (std::declval<int*>(), std::declval<int(*)()>(), int()), 7)
#define SIZE_MISMATCH(X) \
  X(load, (std::declval<uint32_t*>(), std::declval<uint64_t*>(), int()), 8)
#define OUTPUT_CONST(X) \
  X(load, (std::declval<int*>(), std::declval<const int*>(), int()), 9)
#define SECOND_VOLATILE(X) \
  X(load, (std::declval<int*>(), std::declval<volatile int*>(), int()), 10)

#define OTHER_ARG_BADTYPE(X) \
  NONPOINTER_OTHER_ARG(X) \
  SECOND_VOLATILE(X) \
  OUTPUT_CONST(X) \
  SIZE_MISMATCH(X) \
  FUNCTIONPTR_OTHER_ARG(X) \
  NONCONST_SIZE_OTHER_ARG(X)

#define MEMMODEL_BADTYPE(X) \
  X(load, (std::declval<int*>(), std::declval<int*>(), float()), 11)
#define MEMMODEL_TOOLARGE(X) \
  X(load, (std::declval<int*>(), std::declval<int*>(), 100), 12)

#define MEMMODEL_BAD(X) \
  MEMMODEL_BADTYPE(X) \
  MEMMODEL_TOOLARGE(X)

#define GET_ATOMIC_GENERIC_ERRS(X) \
  INCORRECT_NUMBER_ARGUMENTS(X) \
  FIRST_ARGS_BADTYPE(X) \
  OTHER_ARG_BADTYPE(X) \
  MEMMODEL_BAD(X)

#define SYNC_SIZE_TOOFEW(X) \
  X(load_n, (), 0)
#define SYNC_SIZE_INCOMPATIBLE(X) \
  X(load_n, (int(), int()), 1)
#define SYNC_SIZE_ERRS(X) \
  SYNC_SIZE_TOOFEW(X) \
  SYNC_SIZE_INCOMPATIBLE(X)

#define SYNC_PARM_TOOFEW(X) \
  X(load_n, (std::declval<int*>()), 2)
#define SYNC_PARM_TOOMANY(X) \
  X(load_n, (std::declval<int*>(), int(), int()), 3)
#define SYNC_PARM_ERRS(X) \
  SYNC_PARM_TOOFEW(X) \
  SYNC_PARM_TOOMANY(X)

/*
   No Bitint in C++.  Hence can't check for this error.
#define BITINT_FETCHCAS_TOOFEW(X) \
  X(add_fetch, (std::declval<Bitint*>(), std::declval<Bitint>()))
#define BITINT_FETCHCAS_TOOMANY(X) \
  X(add_fetch, (std::declval<Bitint*>(), std::declval<Bitint>(), int(), int()))
#define BITINT_FETCHCAS_ERRS(X) \
  BITINT_FETCHCAS_TOOFEW(X) \
  BITINT_FETCHCAS_TOOMANY(X)
*/
#define BITINT_FETCHCAS_ERRS(X)

#define ALL_ERRS(X) \
  GET_ATOMIC_GENERIC_ERRS(X) \
  SYNC_SIZE_ERRS(X) \
  SYNC_PARM_ERRS(X) \
  BITINT_FETCHCAS_ERRS(X)

#define SFINAE_TYPE_CHECK(NAME, PARAMS, COUNTER) \
  template <typename T, typename = void> \
  struct is_##NAME##_available_##COUNTER : std::false_type {}; \
  template <typename T> \
  struct is_##NAME##_available_##COUNTER<T, \
    std::void_t<decltype(__atomic_##NAME PARAMS) >> \
    : std::true_type {}; \

ALL_ERRS(SFINAE_TYPE_CHECK)

/* { dg-error "too few arguments to function '__atomic_load_n'"                          "" { target *-*-* } 108 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"  "" { target *-*-* } 110 } */
/* { dg-error "too few arguments to function '__atomic_load_n'"                          "" { target *-*-* } 116 } */
/* { dg-error "too many arguments to function '__atomic_load_n'"                         "" { target *-*-* } 118 } */
/* { dg-error "template argument 1 is invalid"                                           "" { target *-*-* } 146 } */
/* { dg-error "template argument 2 is invalid"                                           "" { target *-*-* } 146 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                "" { target *-*-* } 48 } */
/* { dg-error "argument 1 of '__atomic_load' must be a non-void pointer type"            "" { target *-*-* } 51 } */
/* { dg-error "argument 1 of '__atomic_load' must be a non-void pointer type"            "" { target *-*-* } 53 } */
/* { dg-error "argument 1 of '__atomic_load' must be a pointer to a constant size type"  "" { target aarch64_sve } 56 } */
/* { dg-error "argument 1 of '__atomic_load' must be a pointer to a nonzero size object" "" { target *-*-* } 58 } */
/* { dg-error "argument 2 of '__atomic_load' must be a pointer type"                     "" { target *-*-* } 71 } */
/* { dg-error "argument 2 of '__atomic_load' must be a pointer to a constant size type"  "" { target aarch64_sve } 74 } */
/* { dg-error "size mismatch in argument 2 of '__atomic_load'"                           "" { target { ! aarch64_sve } } 74 } */
/* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a function"        "" { target *-*-* } 76 } */
/* { dg-error "size mismatch in argument 2 of '__atomic_load'"                           "" { target *-*-* } 78 } */
/* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a 'const' type"    "" { target *-*-* } 80 } */
/* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a 'volatile' type" "" { target *-*-* } 82 } */
/* { dg-error "non-integer memory model argument 3 of '__atomic_load'"                   "" { target *-*-* } 93 } */

/* { dg-warning {invalid memory model argument 3 of '__atomic_load' \[-Winvalid-memory-model\]} "" { target *-*-* } 95 } */
