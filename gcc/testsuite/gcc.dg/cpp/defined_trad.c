/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-traditional" } */

/* Tests tradcpp0 with defined.  The defined operator in traditional C
   works just the same as the defined operator in Standard C.  */

/* Source: Zack Weinberg, glibc, Neil Booth 11 Dec 2000.  */

#if defined REGPARMS
#error REGPARMS should not be defined
#endif

#define REGPARMS 1
#if !defined REGPARMS
#error REGPARMS should be defined
#endif

#define defined			/* { dg-error "defined" } */

/* No diagnostics, though you could argue there should be.  */
#if defined defined
#error defined is defined!
#endif

#define is_Z_defined defined Z

#if defined Z
#error Z is not defined
#endif

/* The behaviour of "defined" when it comes from a macro expansion is
   now documented.  */
#if is_Z_defined
#error Macro expanding into defined operator test 1
#endif

#define Z

#if !defined Z
#error Z is defined
#endif

#if !is_Z_defined
#error Macro expanding into defined operator test 2
#endif

#undef is_Z_defined
#undef Z

/* Do all the tests over again with the () form of defined.  */

/* No diagnostics, though you could argue there should be.  */
#if defined(defined)
#error defined is defined!
#endif

#define is_Z_defined defined ( Z )

#if defined(Z)
#error Z is not defined
#endif

/* The behaviour of "defined" when it comes from a macro expansion is
   now documented.  */
#if is_Z_defined
#error Macro expanding into defined operator test 1
#endif

#define Z

#if !defined(Z)
#error Z is defined
#endif

#if !is_Z_defined
#error Macro expanding into defined operator test 2
#endif
