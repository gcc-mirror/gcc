struct s {
  double d;
} sd;

struct s g () __attribute__ ((const));

struct s
g ()
{
  return sd;
}

void
f (void)
{
  g ();
}
