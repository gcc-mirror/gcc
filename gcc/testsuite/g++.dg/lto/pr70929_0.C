// { dg-lto-do run }
// { dg-lto-options { "-O3 -flto" } }

struct s
{
  int a;
  s() {a=1;}
  ~s() {}
};
int t(struct s s);
int main()
{
  s s;
  int v=t(s);
  if (!__builtin_constant_p (v))
    __builtin_abort ();
  return 0;
}
