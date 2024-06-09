/* Test C23 typeof and typeof_unqual.  -fno-asm has no effect on keywords in
   C23 mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -fno-asm" } */

int i;
extern typeof (i) i;
