/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests the _Pragma operator.  Contributed by Neil Booth 1 Nov 2000.  */

/* Within the preprocessor, the easy pragma to test is "poison".  */

#pragma GCC poison p1
p1				/* { dg-error "poisoned" } */

/* Standard use of _Pragma.  */
_Pragma ("GCC poison p2")
p2				/* { dg-error "poisoned" } */

/* Don't interpret _Pragmas in directives.  The standard is not clear
   on this, but I think this makes most sense.  */
#if 1 _Pragma (L"GCC poison p3") /* { dg-error "missing binary operator" } */
p3
#endif

#define M1 _Pragma ("GCC poison p4")
p4				/* No problem; not yet poisoned.  */
#define M2(x) _Pragma (#x)

/* Now test macro expansion with embedded _Pragmas.  */
M1 p4				/* { dg-error "poisoned" } */
M2 (GCC poison p5) p5		/* { dg-error "poisoned" } */

/* Not interpreting _Pragma in directives means they don't nest.  */
_Pragma ("_Pragma (\"GCC poison p6\") GCC poison p7")
p6
p7

/* Check we ignore them in false conditionals.  */
#if 0
_Pragma ("GCC poison p8")
#endif
p8				/* No problem.  */
