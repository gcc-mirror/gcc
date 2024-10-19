/* Test -Wtraditional -std=gnu17 does not warn for empty parentheses in
   function definition.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional -std=gnu17" } */

void
f ()
{
}
