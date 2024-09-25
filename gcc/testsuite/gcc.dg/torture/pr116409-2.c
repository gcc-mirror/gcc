/* { dg-do compile } */

int f (int t, char *a, char *b) {
  if (t)
    return __builtin_strlen (a);
  return __builtin_strlen (b);
}
