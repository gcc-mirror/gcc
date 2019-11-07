struct s
{
  int a;
  s() {a=1;}
  ~s() {}
};
int t(struct s s)
{
  return s.a;
}
