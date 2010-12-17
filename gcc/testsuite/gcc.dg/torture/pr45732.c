/* { dg-do compile } */

typedef char chars[5];
const chars bad_chars[] = { "" };

int foo ()
{
  const chars *c = bad_chars;
  return c[0][0];
}
