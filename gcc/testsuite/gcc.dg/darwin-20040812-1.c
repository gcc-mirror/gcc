/* Test dead code strip support.  */
/* Contributed by Devang Patel  <dpatel@apple.com>  */
 
/* { dg-do compile { target *-*-darwin* } } */

const char my_version_string[] __attribute__((__used__))
  = "Do not remove this string\n";
 
 static int
 __attribute__((__used__))
      static_debug_routine()
{
   int i;
   i = 42;
}
 
int
main ()
{
   return 0;
}
 
/* { dg-final { scan-assembler ".no_dead_strip _my_version_string" } } */
/* { dg-final { scan-assembler ".no_dead_strip _static_debug_routine" } } */
