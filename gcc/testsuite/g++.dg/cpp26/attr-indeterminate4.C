// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }
// { dg-additional-options "-ftrivial-auto-var-init=uninitialized -fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not " = \\.DEFERRED_INIT \\\(" "gimple" } }

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
