// Bug: bar is considered to be overloaded (i.e. its
//   IDENTIFIER_GLOBAL_VALUES are TREE_LISTs) even though it isn't,
//   so default_conversion thinks it can't resolve the name.
// Build don't link:

void foo ();
void bar ();

void baz ()
{
  void (*p)() = 1 ? (void (*)()) &foo : bar;
}
