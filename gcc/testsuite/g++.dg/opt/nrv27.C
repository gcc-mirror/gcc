// PR c++/53637
// { dg-additional-options "-Wnrvo -fdump-tree-gimple" }

class Foo {
public:
  Foo() {}
  Foo(const Foo& other);
};

Foo bar(int i) {
  if (i > 1) {
    Foo result;
    return result;	// We currently elide this copy, but not the second one.
    // { dg-final { scan-tree-dump {result .value-expr} "gimple" } }
  } else {
    Foo result;
    return result; // { dg-bogus "not eliding copy on return from" "" { xfail *-*-* } }
  }
}

int main(int argc, char* argv[]) {
  Foo f = bar(argc);
}
