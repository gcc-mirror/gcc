/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests behavior of the defined operator.  */

/*  Source: Neil Booth, 29 Oct 2000, Zack Weinberg 11 Dec 2000.  */

#define defined			/* { dg-error "defined" } */

/* No diagnostics, though you could argue there should be.  */
#if defined defined
#error defined is defined!
#endif

#define is_Z_defined defined Z

#if defined Z
#error Z is not defined
#endif

/* The behavior of "defined" when it comes from a macro expansion is
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

/* The behavior of "defined" when it comes from a macro expansion is
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

/* Use of defined in different contexts.  */

#define bad1 defined
#if !bad1 Z			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

#if !bad1 (Z)			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

#define bad2 defined (Z
#if !bad2)			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

