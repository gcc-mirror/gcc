/* { dg-do compile } */
/* { dg-options "-w" } */
int foo(struct S { int i; } s) {
  return sizeof(struct S); /* { dg-bogus "incomplete type" "S visible here" } */
}
int bar(void) {
  return sizeof(struct S); /* { dg-error "incomplete type" "not here" } */
}
