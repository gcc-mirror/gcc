/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/empty.json -fdump-tuning-model=temp.json" } */

/* { dg-error "expected a JSON value but got EOF"  "" { target *-*-* } 0 } */

int main () {}