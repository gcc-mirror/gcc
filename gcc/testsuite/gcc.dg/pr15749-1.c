/* Flexible array misuses (that are accepted without -pedantic) should
   be OK in system headers even with -pedantic-errors.  PR 15749
   from Tuomo dot Tikkanen at nokia dot com.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#include "pr15749-1.h"
