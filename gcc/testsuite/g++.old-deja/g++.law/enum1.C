// Build don't link: 
// GROUPS passed enums
class X {
  struct X1;
  enum { A= sizeof(X1 *) };     //Causes a problem.
  struct X1 { int i; };
  X1 *f(X1 *);
public:
  X(void);
};

X::X1 *X::f(X1 *x) {
  return x;
}
