// { dg-do assemble  }
// GROUPS passed
template<class T>
class List
{
  public:
    List();
    void f() const;
};

template<class T>
void List<T>::f() const
{
}

void func()
{
    List<int> list;
    list.f();
}
