// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }
// { dg-skip-if "" { c++26 } { "-ftrivial-auto-var-init=*" } { "" } }

struct S { S (); S (const S &); ~S (); int s; };
void foo (S s);							// { dg-message "earlier declaration" }
void bar (S s [[indeterminate]]);
void baz (S s [[indeterminate]]);

void
fred ()
{
  void foo (S t [[indeterminate]]);				// { dg-error "'indeterminate' attribute not specified for parameter 't' on the first declaration of its function" }
  void bar (S t [[indeterminate]]);
  void baz (S t);
  void qux (S t);						// { dg-message "earlier declaration" }
  void corge (S t [[indeterminate]]);
  void garply (S t [[indeterminate]]);
}

void qux (S u [[indeterminate]]);				// { dg-error "'indeterminate' attribute not specified for parameter 'u' on the first declaration of its function" }
void corge (S u [[indeterminate]]);
void garply (S u);
