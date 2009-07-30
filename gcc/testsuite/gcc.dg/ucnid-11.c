/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* *-*-solaris2.* } { "*" } { "" } } */
/* { dg-skip-if "-fdata-sections not supported" { { hppa*-*-hpux* } && { ! hppa*64*-*-* } } { "*" } { "" } } */
/* { dg-options "-std=c99 -fextended-identifiers -fdata-sections" } */

#include "ucnid-3.c"
