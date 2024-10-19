/* Test -Wtraditional -std=gnu23 does not warn for empty parentheses in
   function definition.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional -std=gnu23" } */

void
f ()
{
}
