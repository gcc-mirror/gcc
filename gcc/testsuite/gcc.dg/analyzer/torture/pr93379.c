/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
#include "../../torture/pr57330.c"
/* { dg-warning "use of uninitialized value" "" { target *-*-* } 11 } */
