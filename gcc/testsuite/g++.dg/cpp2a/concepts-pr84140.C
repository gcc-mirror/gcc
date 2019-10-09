// { dg-do run { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<class, class> constexpr bool is_same_v = false;
template<class T> constexpr bool is_same_v<T, T> = true;

template<class T, class U>
concept bool Same = is_same_v<T, U>;

template<class T, class U>
concept bool Diff = requires(T& t, U& u) { u - t; };

template<class I, class S>
int distance(I, S) { return 0; }

template<class I, Diff<I> S>
int distance(I first, S last)
{
  return last - first;
}

template<class T>
struct I
{
  template<class U>
    requires Same<T, U>
  friend int operator-(I const&, I<U> const&)
  {
    static_assert(Same<T, U>);
    return 42;
  }
};

int main()
{
  return distance(I<int>{}, I<void>{});
}

