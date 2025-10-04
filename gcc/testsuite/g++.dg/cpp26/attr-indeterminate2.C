// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-skip-if "" { c++26 } { "-ftrivial-auto-var-init=*" } { "" } }
// Expect .DEFERRED_INIT calls for the h, r and s variables (3) and
// temporaries for the second arguments to foo and baz calls (4).
// { dg-final { scan-tree-dump-times " = \\.DEFERRED_INIT \\\(" 7 "gimple" { target c++26 } } }

struct S { S (); S (const S &); ~S (); int s; };
void foo (S a [[indeterminate]], S b, S c [[indeterminate]] = S ());
void foo (S d, S e, S f [[indeterminate]]);

void
bar ()
{
  S g [[indeterminate]], h;
  foo (g, h, S ());
  foo (g, h);
}

void
foo (S i [[indeterminate]], S j, S k)
{
}

void
baz ([[indeterminate]] S l, S m, [[indeterminate]] S n = S ())
{
}

void baz (S o, S p, S q);

void
qux ()
{
  S r, s;
  baz (r, s, s);
  baz (r, s);
}
