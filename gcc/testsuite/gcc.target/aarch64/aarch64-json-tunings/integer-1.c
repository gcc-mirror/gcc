/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/integer-1.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-final { scan-file "temp.json" "\"sve_width\": 256" } } */
/* { dg-final { scan-file "temp.json" "\"issue_rate\": 4" } } */

int main () {}