/* { dg-do compile { target { { *-*-linux* && { ! ia32 } } && fpic } } } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-O2 -pg -mno-fentry -fpic" } */
/* { dg-message "'-pg' without '-mfentry' may be unreliable with shrink wrapping" "" { target *-*-* } 0 } */
