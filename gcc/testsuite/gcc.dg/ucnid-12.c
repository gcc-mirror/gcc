/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-skip-if "" { ! ucn } { "*" } { "" } } */
/* { dg-skip-if "-ffunction-sections not supported" { { hppa*-*-hpux* } && { ! hppa*64*-*-* } } { "*" } { "" } } */
/* { dg-options "-std=c99 -fextended-identifiers -ffunction-sections" } */

#include "ucnid-4.c"
