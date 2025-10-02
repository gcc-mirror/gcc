/* PR tree-optimization/121962 */
struct s1
{
  int t;
};

struct s2
{
  struct s1 t;
};

struct s1 p;

void f(struct s2 a)
{
  struct s1 *b = &a.t;
  /* this is a nop load/store and should be ignored
     by copy propagation for aggregates.  */
  a.t = *b;
  p = *b;
}
