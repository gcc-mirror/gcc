// PR middle-end/97943
// { dg-do compile }
// { dg-options "" }

struct S { int a; char b[] __attribute__((aligned (2 * sizeof (int)))); }; // { dg-error "flexible array member 'S::b' not at end of 'struct \[TV]'" }
struct T { int a; struct S b; int c; };	// { dg-message "next member 'int T::c' declared here|in the definition of 'struct T'" }
union U { int a; struct S b; };
struct V { int a; union U b; int : 15; int c; };	// { dg-message "next member 'int V::c' declared here|in the definition of 'struct V'" }

void
foo (struct T *t, struct V *v)
{
  __builtin_clear_padding (t);	// { dg-error "flexible array member 'S::b' does not have well defined padding bits for '__builtin_clear_padding'" }
  __builtin_clear_padding (v);	// { dg-error "flexible array member 'S::b' does not have well defined padding bits for '__builtin_clear_padding'" }
}
