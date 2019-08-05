/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mlarge" "-mdata-region=*" } { "" } } */
/* { dg-options "-mcode-region=lower" } */
/* { dg-error "-mcode-region requires the large memory model .-mlarge." "" { target *-*-* } 0 } */
