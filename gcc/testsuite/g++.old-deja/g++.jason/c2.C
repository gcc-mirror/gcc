// PRMS Id: 3134
// g++ understands C redeclaration semantics.  Sun CC 2.0.1 doesn't.
// Special g++ Options:
// Build don't link:

extern "C" {
  int foo();
  int foo(int);

  int bar(int);
  int bar();
}

main()
{
  foo (1);
  bar (1);
}
