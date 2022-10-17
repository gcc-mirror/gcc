/* Test C2x typeof and typeof_unqual.  -fno-asm has no effect on keywords in
   C2x mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -fno-asm" } */

int i;
extern typeof (i) i;
