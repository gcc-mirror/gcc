struct s1 { int f1; };

struct s2 {
  struct s1 a;
  int f2;
};

foo (struct s2 *ptr)
{
  *ptr = (struct s2) {{}, 0};
}
