/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/unidentified-key.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-warning "key .* is not a tuning parameter, skipping"  "" { target *-*-* } 0 } */

int main () {}