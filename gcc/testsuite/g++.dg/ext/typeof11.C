// PR c++/37920

template<typename T> T& ensure_obj(const T&);
template <typename T>
void func2(T& t)
{
  typedef __typeof__(ensure_obj(t)) ttt;
  struct ttt1
  {
    ttt1( ttt arg0 ){}
  }  tttt ( t );
}
int main()
{
  double d = 5;
  func2(d);
}

