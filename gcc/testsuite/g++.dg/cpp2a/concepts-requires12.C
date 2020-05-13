// { dg-do compile { target c++20 } }

template <class> struct all_same {
  static constexpr bool value = true;
};

template <class T> 
concept Assignable = requires(T t)
{
  requires all_same<decltype(t = 0)>::value;
};

template <class I> 
  requires (!Assignable<I>)
int dispatch();

template <class I>
  requires Assignable<I>
void dispatch();

int main() { dispatch<int *>(); }
