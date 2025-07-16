/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/unsigned-3.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-error "key .* value .* is out of range for 'uint' type"  "" { target *-*-* } 0 } */
/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
