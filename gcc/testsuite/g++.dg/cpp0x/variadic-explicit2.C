// PR c++/56774
// { dg-require-effective-target c++11 }

template <class ... Args>
struct mytype {};

template <class T, class ... Args>
void something( mytype<T, Args...> )
{ }

int main()
{
  something<int, char, bool>( mytype<int, char, bool>() );
}
