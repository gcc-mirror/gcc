// PR c++/84724
// { dg-do compile }
// { dg-options "-O3 -fpermissive -w" }

int __builtin_trap ();		// { dg-bogus "ambiguates built-in declaration" }
				// { dg-bogus "ignoring the 'int __builtin_trap\\(\\)' declaration" "" { target *-*-* } .-1 }

int
foo ()
{
  int b;
  int c (&b);			// { dg-bogus "invalid conversion from" }
  return b %= b ? c : 0;
}
