/* PR tree-optimization/83781 - Bootstrap failed on x86 with --with-arch=corei7
   --with-cpu=corei7
   Verify that the upper bound of the size of an array of pointers
   to strings isn't considered to be the upper bound of the lengths
   of the pointed-to strings.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

const char* const ap[32] = { "1", "12", "123" };

char d4[4];
char d7[7];

void nowarn_range_ptr_var_1 (int i)
{
  __builtin_sprintf (d4, "%s", ap[i]);
}

void nowarn_range_ptr_var_2 (int i, int j)
{
  __builtin_sprintf (d7, "%s%s", ap[i], ap[j]);
}
