/* PR69537, spurious warning because of a missed optimization. */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-short-enums -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-21.c"
