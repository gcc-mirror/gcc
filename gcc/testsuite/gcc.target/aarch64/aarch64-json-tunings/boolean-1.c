/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/boolean-1.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-final { scan-file "temp.json" "\"non_exec_costs_exec\": false" } } */

int main () {}