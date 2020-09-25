// PR c++/68828
// { dg-do compile { target c++20 } }

template <typename... Types>
struct Var
{
};

struct A
{
};

template <typename T>
T
forward(T t)
{
  return static_cast<T>(t);
}

template <typename V, typename... Types, typename... Args>
bool requires_types_args(V&& v, Var<Types...>&, Args&&... args)
{
  return (true && ... &&
    requires (V&& v, Types type, Args... args) {
      foo(forward<V>(v), forward<Types>(type), 
        forward<Args>(args)...);
    }
  );
}

void bar()
{
  Var<int, char> v;
  requires_types_args(A(), v, 1, 'b');
}
