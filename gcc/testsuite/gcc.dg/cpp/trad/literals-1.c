/* Test that (what looks like) comments are not recognized in literals
   and that quotes within quotes do not confused the preprocessor.  */

/* { dg-do run } */

extern void abort (void);

int main ()
{
  const char *str1 = "/*";
  const char *str2 = "'";

  if (str1[0] != '/' || str1[1] != '*' || str1[2] != '\0')
    abort ();

  if (str2[0] != '\'' || str2[1] != '\0')
    abort ();

#if '"' != '\"'
#  error /* { dg-bogus "error" "double quote in charconst" } */
#endif

#if !'\''
#  error quote /* { dg-bogus "quote" "quote in charconst" } */
#endif

  return 0;
}
