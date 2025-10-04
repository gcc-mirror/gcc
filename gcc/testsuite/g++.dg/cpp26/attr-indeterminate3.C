// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }
// { dg-skip-if "" { c++26 } { "-ftrivial-auto-var-init=*" } { "" } }

struct S { S (); S (const S &); ~S (); int s; };
void foo (S u, S v [[indeterminate]], int);
void foo (S a, S b, S c = S ());				// { dg-message "earlier declaration" }
void foo (S d, S e, S f [[indeterminate]]);			// { dg-error "'indeterminate' attribute not specified for parameter 'f' on the first declaration of its function" }

void
foo (S i [[indeterminate]], S j, S k)				// { dg-error "'indeterminate' attribute not specified for parameter 'i' on the first declaration of its function" }
{
}

void
bar (S l, S m, S n = S ())					// { dg-message "earlier declaration" }
{
}

void bar (S o [[indeterminate]], S p, [[indeterminate]]S q);	// { dg-error "'indeterminate' attribute not specified for parameter 'o' on the first declaration of its function" }
								// { dg-error "'indeterminate' attribute not specified for parameter 'q' on the first declaration of its function" "" { target *-*-* } .-1 }
