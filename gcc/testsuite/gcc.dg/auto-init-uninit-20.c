/* Spurious uninitialized variable warnings, from gdb */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-20.c"
