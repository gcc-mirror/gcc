// { dg-do compile { target c++20 } }

template<typename I>
struct iterator_traits
{
private:
  template<typename Iter>
  struct ptr
  { };

  template<typename J>
    requires requires { typename J::X; }
  struct ptr<J>
  { };

  template<typename J>
    requires (!requires { typename J::X; } && requires { typename J::Y; })
  struct ptr<J>
  { };
};
