/* { dg-options "-Wno-implicit-function-declaration -Wno-implicit-int -Wno-int-conversion -Wno-incompatible-pointer-types -Wno-return-mismatch" } */

/* This test checks that permerrors can be disabled using -Wno-* options.  */

#include "permerror-default.c"

/* Ideally, we do not want to see any warnings here, but this warning is not
   yet controlled by its own option.  */

/* { dg-warning "parameter names \\\(without types\\\) in function declaration\n" "" { target *-*-* } 22 } */
