/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mlarge" } { "" } } */
/* { dg-options "-mcode-region=either -mdata-region=none" } */
/* { dg-error "-mcode-region and -mdata-region require the large memory model .-mlarge." "" { target *-*-* } 0 } */
