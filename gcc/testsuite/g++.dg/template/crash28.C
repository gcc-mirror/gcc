// PR c++/18445

struct a
{
  int what();
};
void g(void*);
template<class T>
void f()
{
  a ex;
  g(ex.what); // { dg-error "" }
}
