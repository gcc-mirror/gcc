/* PR middle-end/101216 - spurious notes for function calls
   { dg-do compile }
   { dg-options "-O2 -w" } */

__attribute__ ((access (write_only, 1, 2))) char*
getcwd (char *, __SIZE_TYPE__);

char* f (void)
{
  char a[8];
  return getcwd (0, 8);
}

/* Expect no messages of any kind on output.
   { dg-bogus "" "" { target *-*-* } 0 } */
