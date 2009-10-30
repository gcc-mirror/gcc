// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/41863

template<int X>
struct Bar
{
};

template<typename T>
class Foo
{
  T m_foo;

  void
  crash()
  {
    Bar<sizeof(m_foo)> bar;
  }
};
