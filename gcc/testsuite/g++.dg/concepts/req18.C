// { dg-options "-std=c++1z -fconcepts" }

template <class> struct all_same {
  static constexpr bool value = 1;
};
template <class T> concept bool Assignable
= requires(T t)
{
  requires all_same<decltype(t = 0)>::value;
};

template <class I> requires !Assignable<I>
int dispatch();
template <Assignable>
void dispatch();

int main() { dispatch<int *>(); }
