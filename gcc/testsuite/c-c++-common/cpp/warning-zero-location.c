/*
   { dg-options "-D _GNU_SOURCE -fdiagnostics-show-caret" }
   { dg-do compile }
 */

#define _GNU_SOURCE 	/* { dg-warning "-:redefined" } */

/* { dg-message "" "#define _GNU_SOURCE" {target *-*-* } 0 } */
