// PR c++/92268
// { dg-do compile { target c++20 } }

template <class T> concept Two = true;
template <class T> concept One = Two<typename T::type>;
template <class T> concept Zero = requires
  {
   { T() } -> One;
  };

template <class T>
void f() requires Zero<T>;
template <class T>
int f(...);

int main()
{
  f<int>();			// { dg-error "ambiguous" }
}
