
struct s
{
  unsigned a : 8;
  unsigned b : 8;
  unsigned c : 8;
  unsigned d : 8;
};

/*
struct
{
  unsigned a : 8;
  unsigned b : 16;
  unsigned c : 8;
};
*/

struct s
foo (struct s s, int i)
{
  s.b = i;
  return s;
}
