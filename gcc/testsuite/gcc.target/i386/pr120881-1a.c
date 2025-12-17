/* { dg-do compile { target { *-*-linux* && fpic } } } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-O2 -pg -mno-fentry -fno-pic" } */
/* { dg-message "'-pg' without '-mfentry' may be unreliable with shrink wrapping" "" { target *-*-* } 0 } */
