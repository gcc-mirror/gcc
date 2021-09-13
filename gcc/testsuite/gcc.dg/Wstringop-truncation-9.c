/* PR tree-optimization/99489 - ICE calling strncat after strncat
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

// Important -- see pr82429.
char *stpcpy (char *, const char *);

void fchar (char *d, char c, char *s)
{
  __builtin_strcat (d, s);
  __builtin_strncat (d, &c, 1);
}

void fcstchar (char *d, char *s)
{
  __builtin_strcat (d, s);

  const char c = 'x';
  __builtin_strncat (d, &c, 1);     // { dg-warning "-Wstringop-truncation" }
}

void fstr (char *d, char *s)
{
  __builtin_strcat (d, s);
  __builtin_strncat (d, s, 1);
}

void farr (char *d, char *s)
{
  __builtin_strcat (d, s);

  char a[] = "x";
  __builtin_strncat (d, a, 1);      // { dg-warning "-Wstringop-truncation" }
}

void flit (char *d, char *s)
{
  __builtin_strcat (d, s);
  __builtin_strncat (d, "x", 1);    // { dg-warning "-Wstringop-truncation" "pr?????" { xfail *-*-*} }
                                    // { dg-warning "-Wstringop-overflow" "actual" { target *-*-*} .-1 }
}
