// { dg-lto-do link }
// { dg-lto-options { { -flto } { -flto -g } } }

struct T
{
  virtual void m () { }
};
int
main ()
{
  bool fn (T);
  return 0;
}
