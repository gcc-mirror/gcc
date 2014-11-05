/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-skip-if "" { ! ucn } { "*" } { "" } } */
/* { dg-skip-if "-ffunction-sections not supported" { { hppa*-*-hpux* } && { ! lp64 } } { "*" } { "" } } */
/* { dg-options "-std=c99 -ffunction-sections -g" } */

#include "ucnid-4.c"
