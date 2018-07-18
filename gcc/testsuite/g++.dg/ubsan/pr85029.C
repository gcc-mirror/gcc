// PR sanitizer/85029
// { dg-do compile }
// { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } }
// { dg-options "-fsanitize=undefined" }

struct B {
  virtual B bar ();
  int e;
} register a;	// { dg-error "register name not specified for 'a'" }

int
foo (...)
{
  return foo (a);
}
