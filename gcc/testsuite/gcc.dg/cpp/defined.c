/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests behaviour of the defined operator.  */

/*  Source: Neil Booth, 29 Oct 2000.  */

/* No diagnostics, though you could argue there should be.  */
#if defined defined
#error defined is defined!
#endif

#define defined			/* { dg-error "defined" } */

#define is_Z_defined defined Z

/* The behaviour of "defined" when it comes from a macro expansion is
   now documented.  */
#if is_Z_defined		/* { dg-warning "macro expansion" } */
#error Macro expanding into defined operator test 1
#endif

#define Z
#if !is_Z_defined		/* { dg-warning "macro expansion" } */
#error Macro expanding into defined operator test 2
#endif
