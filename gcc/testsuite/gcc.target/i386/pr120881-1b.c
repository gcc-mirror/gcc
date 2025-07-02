/* { dg-do compile { target { fpic && { ! ia32 } } } } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-O2 -pg -mno-fentry -fpic" } */
/* { dg-message "'-pg' without '-mfentry' may be unreliable with shrink wrapping" "" { target *-*-* } 0 } */
