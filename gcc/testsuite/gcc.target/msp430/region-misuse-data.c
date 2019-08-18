/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mlarge" "-mdata-region=*" } { "" } } */
/* { dg-options "-mdata-region=upper" } */
/* { dg-error "-mdata-region requires the large memory model .-mlarge." "" { target *-*-* } 0 } */
