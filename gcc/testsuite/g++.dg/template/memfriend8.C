// { dg-do compile }

// Origin: Martin Sebor <sebor@roguewave.com>

// PR c++/5369: Member function of class template as friend

template <class T>
struct S
{
  int foo () {
    return S<int>::bar ();
  }

private:

  template <class U>
  friend int S<U>::foo ();

  static int bar () { return 0; }
};

int main ()
{
  S<char>().foo ();
}
