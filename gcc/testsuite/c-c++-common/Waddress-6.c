/* PR c/102867 - -Waddress from macro expansion in readelf.c
   { dg-do compile }
   { dg-options "-Wall" } */

#define F(x) ((&x) != 0)

int warn_nomacro (int *p, int i)
{
  return &p[i] != 0;          // { dg-warning "-Waddress" }
}

int nowarn_macro_expansion (int *p, int i)
{
  // Verify that -Waddress isn't issued for code from macro expansion.
  return F (p[i]);            // { dg-bogus "-Waddress" }
}

#define G(x, i) ((&x) + i)

int warn_function_macro_expansion (int *p, int i)
{
  /* Verify that -Waddress is issued for code involving macro expansion
     where the comparison takes place outside the macro.  */
  return G (*p, i) != 0;      // { dg-warning "-Waddress" }
}

#define malloc __builtin_malloc

int warn_object_macro_expansion (int *p, int i)
{
  return malloc != 0;         // { dg-warning "-Waddress" }
}
