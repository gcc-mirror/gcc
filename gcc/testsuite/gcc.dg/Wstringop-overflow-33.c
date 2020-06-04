/* PR middle-end/82456 - missing -Wstringop-overflow on strcpy reading past
   the end of an array
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void fcst (char *d)
{
  char a[2] = "0";

  __builtin_strcpy (d, a + 3);    // { dg-warning "\\\[-W(array-bounds|stringop-overflow)" }
}

void frng (char *d, int i)
{
  char a[2] = "0";

  if (i < 3)
    i = 3;

  __builtin_strcpy (d, a + i);    // { dg-warning "\\\[-W(array-bounds|stringop-overflow)" }
}

void gcst (char *d)
{
  char a[2] = "0";

  __builtin_strcpy (d, a + 2);    // { dg-warning "\\\[-W(array-bounds|stringop-overflow)" }
}

void grng (char *d, int i)
{
  char a[2] = "0";

  if (i < 2)
    i = 2;

  __builtin_strcpy (d, a + i);    // { dg-warning "\\\[-W(array-bounds|stringop-overflow)" }
}

/* { dg-prune-output "-Wuninitialized" } */
