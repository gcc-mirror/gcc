// { dg-require-visibility }
// { dg-do compile }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-assembler-not "\\.hidden\[^\n\]*_Znwj" } }

void f() {
  new int;
}

void *g();

void *operator new(__SIZE_TYPE__) {
  return g();
}
