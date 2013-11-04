// { dg-options "--std=c++11" }
struct S
{
  S();
  S(S &&);
private:
  S(S &);
};

S f()
{
  S s;
  return static_cast<S&&>(s);
}
