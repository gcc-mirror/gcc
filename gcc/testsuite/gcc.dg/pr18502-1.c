/* Test that -trigraphs isn't reordered before -std=gnu99.  Bug
   18502.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -trigraphs" } */

int a??(2??);
