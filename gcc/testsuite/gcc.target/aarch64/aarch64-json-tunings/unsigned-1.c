/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/unsigned-1.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-final { scan-file "temp.json" "\"sve_width\": 512" } } */
/* { dg-final { scan-file "temp.json" "\"extra_tuning_flags\": 16" } } */

int main () {}
