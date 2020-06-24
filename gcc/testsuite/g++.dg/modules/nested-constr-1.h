
template<typename T>
struct traits
{
  template<typename U>
    struct nested
    { using type = void; };

  template<typename U> requires requires { typename U::type; }
    struct nested<U>
    { using type = typename U::type; };
};

using V = traits<char>::nested<int>::type;

