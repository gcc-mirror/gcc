#include "inc-from-1b.h"

/* { dg-error "" ""  { target *-*-* } 1 } */

/* { dg-regexp "In file included from \[^\n]*inc-from-1b.h:1,\n *from \[^\n]*inc-from-1.c:1:\n" } */
