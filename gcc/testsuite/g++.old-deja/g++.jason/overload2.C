// { dg-do assemble  }
// Bug: foo and bar are considered to be overloaded (i.e. their
//   IDENTIFIER_GLOBAL_VALUES are TREE_LISTs) even though they aren't,
//   so ?: thinks it can't resolve the names.

void foo ();
void bar ();

void baz ()
{
  void (*p)() = 1 ? foo : bar;	// { dg-bogus "" } wrongful overloading
}
