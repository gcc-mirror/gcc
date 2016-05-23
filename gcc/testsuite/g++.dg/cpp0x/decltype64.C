// PR c++/53401
// { dg-do compile { target c++11 } }

template<int I>
struct index
{};

constexpr int recursive_impl(index<0u>)
{
  return 0;
}

template<int N>
constexpr auto recursive_impl(index<N>)
  -> decltype(recursive_impl(index<N - 1>()))  // { dg-error "depth" }
{
  return recursive_impl(index<N - 1>());
}

template<int N>
constexpr auto recursive()
  -> decltype(recursive_impl(index<N>()))
{
  return recursive_impl(index<N>());
}

void f(int i)
{
  recursive<1>();   // { dg-message "from here" }
}

// { dg-prune-output "compilation terminated" }
