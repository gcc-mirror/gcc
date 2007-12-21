/* { dg-do compile } */

struct s {
  char c;
};

struct s
foo (void)
{
  struct s s = { 0 };
  return s;
}
