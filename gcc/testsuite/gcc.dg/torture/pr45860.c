/* { dg-do compile } */

void
foo (char *str, int i)
{
  static const char text[] = "";
  str[i] = 0;
  if (i & 1)
    __builtin_strcpy (str + i, text);
}

