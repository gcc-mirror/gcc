// PR c++/84724
// { dg-do compile }
// { dg-options "-O3 -fpermissive" }

int __builtin_trap ();		// { dg-warning "ambiguates built-in declaration" }
				// { dg-message "ignoring the 'int __builtin_trap\\(\\)' declaration" "" { target *-*-* } .-1 }

int
foo ()
{
  int b;
  int c (&b);			// { dg-warning "invalid conversion from" }
  return b %= b ? c : 0;
}
