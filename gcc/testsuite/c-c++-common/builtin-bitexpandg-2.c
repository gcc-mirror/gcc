/* Test __builtin_bitexpandg.  */
/* { dg-do compile } */
/* { dg-options "" } */

int
main ()
{
  unsigned int a;
  __builtin_bitexpandg ();			/* { dg-error "wrong number of arguments to '__builtin_bitexpandg'" } */
  __builtin_bitexpandg (1U);			/* { dg-error "wrong number of arguments to '__builtin_bitexpandg'" } */
  __builtin_bitexpandg (1U, 1U, &a);		/* { dg-error "wrong number of arguments to '__builtin_bitexpandg'" } */
  __builtin_bitexpandg (1U, 1U, &a, 1U);	/* { dg-error "wrong number of arguments to '__builtin_bitexpandg'" } */
  __builtin_bitexpandg (1.0, 1.0);		/* { dg-error "'__builtin_bitexpandg' operand not an integral type" } */
  __builtin_bitexpandg (1.0f, 1.0f);		/* { dg-error "'__builtin_bitexpandg' operand not an integral type" } */
  __builtin_bitexpandg (1.0L, 1.0L);		/* { dg-error "'__builtin_bitexpandg' operand not an integral type" } */
  struct S { int a; };
  __builtin_bitexpandg ((struct S) { 42 }, (struct S) { 43 }); /* { dg-error "'__builtin_bitexpandg' operand not an integral type" } */
  enum E { E0, E1 } e;
  __builtin_bitexpandg (E1, E1);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has signed type" "" { target c } } */
						/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" "" { target c++ } .-1 } */
  __builtin_bitexpandg (e, e);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" } */
  __builtin_bitexpandg ((enum E) 1, (enum E) 1); /* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" } */
#if !defined(__cplusplus) || __cplusplus >=  201103L
  enum F : int { F0, F1 } f;
  __builtin_bitexpandg (F1, F1);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" "" { target { c || c++11 } } } */
  __builtin_bitexpandg (f, f);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" "" { target { c || c++11 } } } */
  __builtin_bitexpandg ((enum F) 1, (enum F) 1); /* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has enumerated type" "" { target { c || c++11 } } } */
#endif
  __builtin_bitexpandg (false, true);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has boolean type" } */
  __builtin_bitexpandg (1, 1);		/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has signed type" } */
  __builtin_bitexpandg ((char) 1, (char) 1);	/* { dg-error "argument 1 in call to function '__builtin_bitexpandg' has signed type" "" { target { ! unsigned_char } } } */
  __builtin_bitexpandg (1UL, 1ULL);		/* { dg-error "arguments 1 and 2 to function '__builtin_bitexpandg' have different types 'long unsigned int' and 'long long unsigned int'" } */
  __builtin_bitexpandg (1U, 1.0);		/* { dg-error "arguments 1 and 2 to function '__builtin_bitexpandg' have different types 'unsigned int' and 'double'" } */
  __builtin_bitexpandg ((unsigned char) 1, (char) 1); /* { dg-error "arguments 1 and 2 to function '__builtin_bitexpandg' have different types 'unsigned char' and 'char'" } */
  __builtin_bitexpandg ((unsigned char) 1, true); /* { dg-error "arguments 1 and 2 to function '__builtin_bitexpandg' have different types 'unsigned char' and 'bool'" } */
}
