// { dg-do link  }
// GROUPS passed templates
class ostream {};

template <class T>
class S;

template <class T>
void operator<<(ostream&, S<T>) {}

template <class T>
class S
{
  friend void operator<<<>(ostream&, const S<T>);
};


int main()
{
  ostream o;

  o << S<int>();
}
