// PR c++/119518
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump "S::~S \\\(&s\\\)" "optimized" } }

[[gnu::noipa, noreturn]] void
foo ()
{
  for (;;)
    ;
}

struct S { ~S (); };

void
bar ()
{
  S s;
  foo ();
}
