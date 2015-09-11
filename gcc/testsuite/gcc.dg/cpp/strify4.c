/* { dg-do run } */
/* Tests we stringify without changing unprintable characts.  

   Andrew Pinski */

extern int strcmp (const char *, const char *);
#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)


#define S(X) S2(X)
#define S2(X) #X
#define TAB "	" /* Note there is a tab character here. */

int main (int argc, char *argv[])
{
  /* The space before "bar" here is vital.  */
  char a[] = S(S(TAB));

  if (strcmp (a, "\"\\\"	\\\"\""))
    err ("stringification caused octal");

  return 0;
}
