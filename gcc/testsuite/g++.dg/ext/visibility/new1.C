// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-not-hidden "_Znwj" } }

void f() {
  new int;
}

void *g();

void *operator new(__SIZE_TYPE__) {
  return g();
}
