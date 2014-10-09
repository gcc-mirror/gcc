/* Test that whitespace in arguments is preserved, and that each
   newline in macro arguments become a space.  */

/* { dg-do run } */

#define f(x, y) "x y"
#define g(x) x

extern void abort (void);
extern int strcmp (const char *, const char *);

void testquoting ()
{
  const char *str1 = f("a", "\"a\"");
  const char *str2 = f( \t, " \t");

  if (strcmp (str1, "\"a\"  \"\\\"a\\\"\""))
    abort ();
  if (strcmp (str2, " \t  \" \\t\""))
    abort ();
}

int main ()
{
  const char *str1 = f( foo ,bar);
  const char *str2 = f(
foo
,bar);

  if (strcmp (str1, " foo  bar"))
    abort ();

  if (strcmp (str1, str2))
    abort ();

  /* Verify that quoted state is preserved over a newline.  */
  if (strcmp (g /* { dg-bogus "unterminated 2" } */ ("1
, 2"), "1 , 2"))	
    abort ();

  testquoting ();

  return 0;
}
