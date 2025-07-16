/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/string-1.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-final { scan-file "temp.json" "\"function_align\": \"16\"" } } */
/* { dg-final { scan-file "temp.json" "\"jump_align\": \"2\"" } } */
/* { dg-final { scan-file "temp.json" "\"loop_align\": \"8\"" } } */

int main () {}