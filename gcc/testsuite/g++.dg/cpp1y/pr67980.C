// { dg-do compile { target c++14 } }

template <int Y, class T>
constexpr T cpp14_constexpr_then(T value) {
  if (Y < 0)
    return (value << -Y);
  else
    return 0;
}

template <int Y, class T>
constexpr T cpp14_constexpr_else(T value) {
  if (Y > 0)
    return 0;
  else
    return (value << -Y);
}

int main()
{
  cpp14_constexpr_then<1>(0);
  cpp14_constexpr_else<1>(0);
}
