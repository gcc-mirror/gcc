// { dg-additional-options "-fmodules-ts" }
import Foo;
import Bar;


void Three ()
{
  Frob (0L, 0, 0);

  Frob (0, 0, 0); // { dg-error "ambiguous" }
  // { dg-regexp {candidate 1: 'int Frob@Foo\(int, long int, int\)'} }
  // { dg-regexp {candidate 2: 'int Frob@Foo\(int, int, long int\)'} }
  // { dg-regexp {candidate 3: 'int Frob@Bar\(long int, int, int\)'} }
}
