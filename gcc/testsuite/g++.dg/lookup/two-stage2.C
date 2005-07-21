// { dg-do compile }
// PR c++/2922

char& f(char);

template<class T>
void g(T t)
{
  char& c1 = f(1);        // not dependent
  char& c2 = f(t);        // dependent
}

int&f (int);

int main()
{
  g(2);    // f(char) followed by f(int)
  g('a');  // two f(char)
}
