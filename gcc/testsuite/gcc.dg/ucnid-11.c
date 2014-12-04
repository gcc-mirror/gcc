/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-skip-if "" { ! ucn } { "*" } { "" } } */
/* { dg-skip-if "-fdata-sections not supported" { { hppa*-*-hpux* } && { ! lp64 } } { "*" } { "" } } */
/* { dg-options "-std=c99 -fdata-sections -g" } */

#include "ucnid-3.c"
