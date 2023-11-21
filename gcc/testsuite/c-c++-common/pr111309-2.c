/* PR c/111309 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c99" { target c } } */

#ifndef __cplusplus
#define bool _Bool
#define true ((_Bool) 1)
#define false ((_Bool) 0)
#endif

void
foo (void)
{
  enum E { E0 = 0 };
  struct S { int s; } s;
  __builtin_clzg ();		/* { dg-error "too few arguments" } */
  __builtin_clzg (0U, 1, 2);	/* { dg-error "too many arguments" } */
  __builtin_clzg (0);		/* { dg-error "has signed type" } */
  __builtin_clzg (0.0);		/* { dg-error "does not have integral type" } */
  __builtin_clzg (s);		/* { dg-error "does not have integral type" } */
  __builtin_clzg (true);	/* { dg-error "has boolean type" } */
  __builtin_clzg (E0);		/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
  __builtin_clzg (0, 0);	/* { dg-error "has signed type" } */
  __builtin_clzg (0.0, 0);	/* { dg-error "does not have integral type" } */
  __builtin_clzg (s, 0);	/* { dg-error "does not have integral type" } */
  __builtin_clzg (true, 0);	/* { dg-error "has boolean type" } */
  __builtin_clzg (E0, 0);	/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
  __builtin_clzg (0U, 2.0);	/* { dg-error "does not have integral type" } */
  __builtin_clzg (0U, s);	/* { dg-error "does not have integral type" } */
  __builtin_clzg (0U, 2LL);	/* { dg-error "does not have 'int' type" } */
  __builtin_clzg (0U, 2U);	/* { dg-error "does not have 'int' type" } */
  __builtin_clzg (0U, true);
  __builtin_clzg (0U, E0);	/* { dg-error "does not have 'int' type" "" { target { c++ && { ! short_enums } } } } */
  __builtin_ctzg ();		/* { dg-error "too few arguments" } */
  __builtin_ctzg (0U, 1, 2);	/* { dg-error "too many arguments" } */
  __builtin_ctzg (0);		/* { dg-error "has signed type" } */
  __builtin_ctzg (0.0);		/* { dg-error "does not have integral type" } */
  __builtin_ctzg (s);		/* { dg-error "does not have integral type" } */
  __builtin_ctzg (true);	/* { dg-error "has boolean type" } */
  __builtin_ctzg (E0);		/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
  __builtin_ctzg (0, 0);	/* { dg-error "has signed type" } */
  __builtin_ctzg (0.0, 0);	/* { dg-error "does not have integral type" } */
  __builtin_ctzg (s, 0);	/* { dg-error "does not have integral type" } */
  __builtin_ctzg (true, 0);	/* { dg-error "has boolean type" } */
  __builtin_ctzg (E0, 0);	/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
  __builtin_ctzg (0U, 2.0);	/* { dg-error "does not have integral type" } */
  __builtin_ctzg (0U, 2LL);	/* { dg-error "does not have 'int' type" } */
  __builtin_ctzg (0U, 2U);	/* { dg-error "does not have 'int' type" } */
  __builtin_ctzg (0U, true);
  __builtin_ctzg (0U, E0);	/* { dg-error "does not have 'int' type" "" { target { c++ && { ! short_enums } } } } */
  __builtin_clrsbg ();		/* { dg-error "too few arguments" } */
  __builtin_clrsbg (0, 1);	/* { dg-error "too many arguments" } */
  __builtin_clrsbg (0U);	/* { dg-error "has unsigned type" } */
  __builtin_clrsbg (0.0);	/* { dg-error "does not have integral type" } */
  __builtin_clrsbg (s);		/* { dg-error "does not have integral type" } */
  __builtin_clrsbg (true);	/* { dg-error "has boolean type" } */
  __builtin_clrsbg (E0);	/* { dg-error "has enumerated type" "" { target c++ } } */
  __builtin_ffsg ();		/* { dg-error "too few arguments" } */
  __builtin_ffsg (0, 1);	/* { dg-error "too many arguments" } */
  __builtin_ffsg (0U);		/* { dg-error "has unsigned type" } */
  __builtin_ffsg (0.0);		/* { dg-error "does not have integral type" } */
  __builtin_ffsg (s);		/* { dg-error "does not have integral type" } */
  __builtin_ffsg (true);	/* { dg-error "has boolean type" } */
  __builtin_ffsg (E0);		/* { dg-error "has enumerated type" "" { target c++ } } */
  __builtin_parityg ();		/* { dg-error "too few arguments" } */
  __builtin_parityg (0U, 1);	/* { dg-error "too many arguments" } */
  __builtin_parityg (0);	/* { dg-error "has signed type" } */
  __builtin_parityg (0.0);	/* { dg-error "does not have integral type" } */
  __builtin_parityg (s);	/* { dg-error "does not have integral type" } */
  __builtin_parityg (true);	/* { dg-error "has boolean type" } */
  __builtin_parityg (E0);	/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
  __builtin_popcountg ();	/* { dg-error "too few arguments" } */
  __builtin_popcountg (0U, 1);	/* { dg-error "too many arguments" } */
  __builtin_popcountg (0);	/* { dg-error "has signed type" } */
  __builtin_popcountg (0.0);	/* { dg-error "does not have integral type" } */
  __builtin_popcountg (s);	/* { dg-error "does not have integral type" } */
  __builtin_popcountg (true);	/* { dg-error "has boolean type" } */
  __builtin_popcountg (E0);	/* { dg-error "has signed type" "" { target c } } */
				/* { dg-error "has enumerated type" "" { target c++ } .-1 } */
}
