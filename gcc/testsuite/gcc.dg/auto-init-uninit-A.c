/* Inspired by part of java/parse.y.
   May be a real bug in CSE. */

/* { dg-do compile } */
/* { dg-options "-O2 -Wall -ftrivial-auto-var-init=zero" } */

#include "uninit-A.c"
