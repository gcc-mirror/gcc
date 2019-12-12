/* Test case to check if multiversioned functions are still generated if they are
   marked comdat with inline keyword.  */

/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O2" } */


/* Default version.  */
inline int __attribute__ ((target ("default")))
foo ()
{
  return 0;
}

inline int __attribute__ ((target ("popcnt")))
foo ()
{
  return 0;
}

int main ()
{
  return foo ();
}
