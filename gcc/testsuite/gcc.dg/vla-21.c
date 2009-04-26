/* Type names for VLAs should be allowed outside functions if the size
   is not evaluated.  PR 39581.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

int a;
int b = sizeof (int (*)[a]);
