/* { dg-lto-do link } */
extern int **foo (void);
extern void mumble (char*, char*, char*);

static int *
bar (char **sp)
{
 char *s = *sp, *rs = s;
 int c;
 while (*foo ()[c])
   rs++;
 while (c = *rs)
   {
     if (c || ((c == '"') || (c == '\'')))
       {
         if (c)
           *rs++ = c;
         else
           mumble (0, "", "");
       }
     else if (c || (*foo ()[c] & 1))
       *rs++ = c;
   }
   if (c)
     mumble (0, "", "");
}

static void
baz (char *s)
{
  char *args[100];
  while (bar (&s))
   {
     mumble (args[0], "", "");
   }
}

int
main (void)
{
 baz ("");
 return 0;
}
