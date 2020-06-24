// { dg-additional-options "-std=c++2a -fmodules-ts" }
export module foo;
// { dg-module-cmi foo }

export template<typename T>
struct traits
{
  template<typename U>
    struct nested
    { using type = void; };

  template<typename U> requires requires { typename U::type; }
    struct nested<U>
    { using type = typename U::type; };
};

export using V = traits<char>::nested<int>::type;

