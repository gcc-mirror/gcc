// Build don't link:

template <class T>
struct S1
{
  void foo(T = t());

  static T t();
};


template <class T>
struct S2
{
  void bar();
};


template <class T>
void S2<T>::bar ()
{
  S1<T> st;
  st.foo();
}


int main()
{
  S2<int> s2i;
  s2i.bar();
}

