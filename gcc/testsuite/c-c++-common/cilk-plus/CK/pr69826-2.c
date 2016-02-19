/* { dg-do run { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus -save-temps" } */
/* { dg-additional-options "-std=gnu99" { target c } } */
/* { dg-additional-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#include "pr69826-1.c"
