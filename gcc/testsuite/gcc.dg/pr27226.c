/* { dg-do compile } */
/* { dg-options "-O2" } */

char *strcpy (char *, const char *);

extern void g ();

f ()
{
  struct {
    int i;
    char str[31];
  } s;

  strcpy (s.str, "text text text text text text text text");
  g (s.str);
}

/* { dg-final { scan-assembler-not "memcpy" } } */
