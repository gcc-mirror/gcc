/* { dg-require-effective-target alloca } */
/* PR target/78439.  */

enum demangle_component_type
{
  DEMANGLE_COMPONENT_THROW_SPEC
};
struct demangle_component
{
  enum demangle_component_type type;
  struct
  {
    struct
    {
      struct demangle_component *left;
      struct demangle_component *right;
    };
  };
};

int a, b;

struct d_info
{
  struct demangle_component *comps;
  int next_comp;
  int num_comps;
  struct demangle_component *subs;
  int num_subs;
  int is_conversion;
};

void
fn1 (int p1, struct d_info *p2)
{
  p2->num_comps = 2 * p1;
  p2->next_comp = p2->num_subs = p1;
  p2->is_conversion = 0;
}

int fn3 (int *);
void fn4 (struct d_info *, int);

void
fn2 ()
{
  int c;
  struct d_info d;
  b = 0;
  c = fn3 (&a);
  fn1 (c, &d);
  struct demangle_component e[d.num_comps];
  struct demangle_component *f[d.num_subs];
  d.comps = e;
  d.subs = (struct demangle_component *) f;
  fn4 (&d, 1);
}
