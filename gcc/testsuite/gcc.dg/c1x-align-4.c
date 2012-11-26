/* Test C1X alignment support.  Test reducing alignment (assumes there
   are at least some alignment constraints).  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */
/* { dg-skip-if "no alignment constraints" { "avr-*-*" } { "*" } { "" } } */

#include <stddef.h>

_Alignas (_Alignof (char)) max_align_t x; /* { dg-error "reduce alignment" } */
