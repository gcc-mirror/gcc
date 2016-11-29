/* PR c/77520 - wrong value for extended ASCII characters in -Wformat message
   Verify that characters in the extended ASCII range are quoted and not
   allowed to be printed raw.  */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

void f (void)
{
  __builtin_printf ("%\x80");   /* { dg-warning "unknown conversion type character .\\\\x80. in format" } */
}
