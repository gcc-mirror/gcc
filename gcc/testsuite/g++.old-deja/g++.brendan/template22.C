// GROUPS passed templates
extern "C" int printf (const char *, ...);

template <class T>
class Foo
{
public:
  void func (int const& i);
};

template <class T>
void Foo<T>::
func (int const& i)
{}


int main ()
{ 
  Foo<int const> foo;
  printf ("PASS\n");
  return 0;
}
