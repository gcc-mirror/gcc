// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/10849: Incorrect access checking on template specialization.

class X {
  private:
    template <typename T> struct Y;
};

template <> struct X::Y<int> {};

template <typename T> struct X::Y {};

template struct X::Y<int>;
