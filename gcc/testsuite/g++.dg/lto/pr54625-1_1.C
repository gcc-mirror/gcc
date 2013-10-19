extern "C" double sin (double);
typedef double UnaryFunType (double);
class A
{
public:
  int hash ();
  double lookup (UnaryFunType p1)
    {
      int a = hash ();
      if (p1)
	return 0;
    }
};
A b;
void
math_sin_impl ()
{
  b.lookup (sin);
}
