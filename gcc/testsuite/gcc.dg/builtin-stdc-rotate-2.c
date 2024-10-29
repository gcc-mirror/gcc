/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  typedef int V __attribute__ ((vector_size (4 * sizeof (int))));
  struct S { int s; };
  enum E { E0, E1 };
  __builtin_stdc_rotate_left (0.0f, 0);			/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0.0, 0);			/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0.0L, 0);			/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left ((V) {}, 0);		/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left ((struct S) { 0 }, 0);	/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left ();			/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_left'" } */
  __builtin_stdc_rotate_left (0U);			/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_left'" } */
  __builtin_stdc_rotate_left (0U, 0U, 0U);		/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_left'" } */
  __builtin_stdc_rotate_left ((_Bool) 0, 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_left' has boolean type" } */
  __builtin_stdc_rotate_left ((enum E) E0, 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_left' has enumerated type" } */
  __builtin_stdc_rotate_left (0, 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_left' has signed type" } */
  __builtin_stdc_rotate_left (0U, 0.0f);		/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0U, 0.0);			/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0U, 0.0L);		/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0U, (V) {});		/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0U, (struct S) { 0 });	/* { dg-error "'__builtin_stdc_rotate_left' operand not an integral type" } */
  __builtin_stdc_rotate_left (0U, (enum E) E0);		/* { dg-error "argument 2 in call to function '__builtin_stdc_rotate_left' has enumerated type" } */
  __builtin_stdc_rotate_right (0.0f, 0);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0.0, 0);			/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0.0L, 0);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right ((V) {}, 0);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right ((struct S) { 0 }, 0);	/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right ();			/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_right'" } */
  __builtin_stdc_rotate_right (0U);			/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_right'" } */
  __builtin_stdc_rotate_right (0U, 0U, 0U);		/* { dg-error "wrong number of arguments to '__builtin_stdc_rotate_right'" } */
  __builtin_stdc_rotate_right ((_Bool) 0, 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_right' has boolean type" } */
  __builtin_stdc_rotate_right ((enum E) E0, 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_right' has enumerated type" } */
  __builtin_stdc_rotate_right (0, 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_right' has signed type" } */
  __builtin_stdc_rotate_right (0U, 0.0f);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0U, 0.0);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0U, 0.0L);		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0U, (V) {});		/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0U, (struct S) { 0 });	/* { dg-error "'__builtin_stdc_rotate_right' operand not an integral type" } */
  __builtin_stdc_rotate_right (0U, (enum E) E0);	/* { dg-error "argument 2 in call to function '__builtin_stdc_rotate_right' has enumerated type" } */
  __builtin_stdc_rotate_left ((unsigned char) 0, -1);	/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_right ((unsigned char) 0, -1);	/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_left ((unsigned short) 0, -1);	/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_right ((unsigned short) 0, -1);	/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_left (0U, -1);			/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_right (0U, -1);			/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_left (0UL, -1);			/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_right (0UL, -1);		/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_left (0ULL, -1);		/* { dg-warning "rotate count is negative" } */
  __builtin_stdc_rotate_right (0ULL, -1);		/* { dg-warning "rotate count is negative" } */
#ifdef __SIZEOF_INT128__
  __builtin_stdc_rotate_left ((unsigned __int128) 0, -1); /* { dg-warning "rotate count is negative" "" { target int128 } } */
  __builtin_stdc_rotate_right ((unsigned __int128) 0, -1); /* { dg-warning "rotate count is negative" "" { target int128 } } */
#endif
#if __BITINT_MAXWIDTH__ >= 64
  __builtin_stdc_rotate_left (0uwb, -1);		/* { dg-warning "rotate count is negative" "" { target bitint } } */
  __builtin_stdc_rotate_right (0uwb, -1);		/* { dg-warning "rotate count is negative" "" { target bitint } } */
  __builtin_stdc_rotate_left ((unsigned _BitInt(2)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint } } */
  __builtin_stdc_rotate_right ((unsigned _BitInt(2)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint } } */
  __builtin_stdc_rotate_left ((unsigned _BitInt(59)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint } } */
  __builtin_stdc_rotate_right ((unsigned _BitInt(59)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint } } */
#endif
#if __BITINT_MAXWIDTH__ >= 575
  __builtin_stdc_rotate_left ((unsigned _BitInt(525)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint575 } } */
  __builtin_stdc_rotate_right ((unsigned _BitInt(525)) 0, -1); /* { dg-warning "rotate count is negative" "" { target bitint575 } } */
#endif
}
