/* Verify we don't ICE on this case with these options.  */

/* { dg-additional-options "-fanalyzer-call-summaries --param=analyzer-max-svalue-depth=0 -Wno-analyzer-symbol-too-complex" } */

int foo_i;
void bar() {}
void foo() {
  if (foo_i)
    bar();
  else
    goto f1;
  bar();
f1:
  bar();
}
int main() {
  foo();
  foo();
  return 0;
}
