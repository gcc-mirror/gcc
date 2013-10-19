extern "C" double sin (double);
typedef double (*UnaryFunType) (double);
class A
{
public:
  int hash ();
  void lookup (UnaryFunType p1)
    {
      int a = hash ();
      p1 (0);
    }
};
A b, c;
void
math_sin_impl ()
{
  b.lookup (sin);
}

void
js_math_sqrt ()
{
  c.lookup (0);
}
