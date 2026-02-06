/* PR target/123489 */
/* { dg-do compile } */
/* { dg-options "-mavx10.2" } */

typedef double v2df __attribute__((__vector_size__(16)));
typedef float v4sf __attribute__((__vector_size__(16)));

long long
foo ()
{
  return __builtin_ia32_cvttsd2sis64_round ((v2df) {}, 8)	/* { dg-error "implicit declaration of function" "" { target ia32 } } */
	 + __builtin_ia32_cvttsd2usis64_round ((v2df) {}, 8);	/* { dg-error "implicit declaration of function" "" { target ia32 } } */
}

long long
bar ()
{
  return __builtin_ia32_cvttss2sis64_round ((v4sf) {}, 8)	/* { dg-error "implicit declaration of function" "" { target ia32 } } */
	 + __builtin_ia32_cvttss2usis64_round ((v4sf) {}, 8);	/* { dg-error "implicit declaration of function" "" { target ia32 } } */
}
