//Overload resolution should consider both declarations of func identically.

struct S{};
void func(S&){}

int main()
{
  void func(S&);
  S s;
  func(s);
}

