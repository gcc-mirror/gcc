/* Test __builtin_clmulg.  */
/* { dg-do compile } */
/* { dg-options "" } */

int
main ()
{
  unsigned int a;
  __builtin_clmulg ();			/* { dg-error "wrong number of arguments to '__builtin_clmulg'" } */
  __builtin_clmulg (1U);		/* { dg-error "wrong number of arguments to '__builtin_clmulg'" } */
  __builtin_clmulg (1U, 1U, &a, 1U);	/* { dg-error "wrong number of arguments to '__builtin_clmulg'" } */
  __builtin_clmulg (1.0, 1.0);		/* { dg-error "'__builtin_clmulg' operand not an integral type" } */
  __builtin_clmulg (1.0f, 1.0f);	/* { dg-error "'__builtin_clmulg' operand not an integral type" } */
  __builtin_clmulg (1.0L, 1.0L);	/* { dg-error "'__builtin_clmulg' operand not an integral type" } */
  struct S { int a; };
  __builtin_clmulg ((struct S) { 42 }, (struct S) { 43 }); /* { dg-error "'__builtin_clmulg' operand not an integral type" } */
  enum E { E0, E1 } e;
  __builtin_clmulg (E1, E1);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has signed type" "" { target c } } */
					/* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" "" { target c++ } .-1 } */
  __builtin_clmulg (e, e);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" } */
  __builtin_clmulg ((enum E) 1, (enum E) 1); /* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" } */
#if !defined(__cplusplus) || __cplusplus >=  201103L
  enum F : int { F0, F1 } f;
  __builtin_clmulg (F1, F1);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" "" { target { c || c++11 } } } */
  __builtin_clmulg (f, f);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" "" { target { c || c++11 } } } */
  __builtin_clmulg ((enum F) 1, (enum F) 1); /* { dg-error "argument 1 in call to function '__builtin_clmulg' has enumerated type" "" { target { c || c++11 } } } */
#endif
  __builtin_clmulg (false, true);	/* { dg-error "argument 1 in call to function '__builtin_clmulg' has boolean type" } */
  __builtin_clmulg (1, 1);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has signed type" } */
  __builtin_clmulg ((char) 1, (char) 1);/* { dg-error "argument 1 in call to function '__builtin_clmulg' has signed type" "" { target { ! unsigned_char } } } */
  __builtin_clmulg (1UL, 1ULL);		/* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'long unsigned int' and 'long long unsigned int'" } */
  __builtin_clmulg (1U, 1.0);		/* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned int' and 'double'" } */
  __builtin_clmulg ((unsigned char) 1, (char) 1); /* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned char' and 'char'" } */
  __builtin_clmulg ((unsigned char) 1, true); /* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned char' and 'bool'" } */
  __builtin_clmulg (1UL, 1UL, 1UL);	/* { dg-error "argument 3 to function '__builtin_clmulg' does not have pointer type" } */
  __builtin_clmulg (1UL, 1UL, &a);	/* { dg-error "arguments 1 and 3 to function '__builtin_clmulg' have incompatible types 'long unsigned int' and 'unsigned int ?\\\*'" } */
  const unsigned int b = 42;
  __builtin_clmulg (1U, 1U, &b);	/* { dg-error "argument 3 to function '__builtin_clmulg' has pointer to 'const' type 'const unsigned int ?\\\*'" } */
  bool c = false;
  int d = 1;
  char g = ' ';
  unsigned long h = 42;
  unsigned char i = 1;
  __builtin_clmulg (false, true, &c);	/* { dg-error "argument 1 in call to function '__builtin_clmulg' has boolean type" } */
  __builtin_clmulg (1, 1, &d);		/* { dg-error "argument 1 in call to function '__builtin_clmulg' has signed type" } */
  __builtin_clmulg ((char) 1, (char) 1, &g); /* { dg-error "argument 1 in call to function '__builtin_clmulg' has signed type" "" { target { ! unsigned_char } } } */
  __builtin_clmulg (1UL, 1ULL, &h);	/* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'long unsigned int' and 'long long unsigned int'" } */
  __builtin_clmulg (1U, 1.0, &a);	/* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned int' and 'double'" } */
  __builtin_clmulg ((unsigned char) 1, (char) 1, &i); /* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned char' and 'char'" } */
  __builtin_clmulg ((unsigned char) 1, true, &i); /* { dg-error "arguments 1 and 2 to function '__builtin_clmulg' have different types 'unsigned char' and 'bool'" } */
#ifndef __cplusplus
  _Atomic unsigned int j = 42;
  __builtin_clmulg (1U, 2U, &j);	/* { dg-error "argument 3 to function '__builtin_clmulg' has pointer to '_Atomic' type '_Atomic unsigned int \\\*'" "" { target c } } */
#endif
}
