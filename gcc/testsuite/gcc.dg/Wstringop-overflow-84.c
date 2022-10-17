/* PR middle-end/102960 - ICE: in sign_mask, at wide-int.h:855 in GCC 10.3.0
   { dg-do compile }
   { dg-options "-Og -Wall" } */

void f (int i)
{
  const char *s;
  if (i)
    s = &"abcd"[i];

  __builtin_printf ("%s", s);
}

/* The use of s in the call to sprintf should result in:
   { dg-prune-output "-Wmaybe-uninitialized" } */
