// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/10849: Incorrect access checking on class template partial
// specialization.

class X {
  private:
    template <typename T> struct Y;
};

template <typename T> struct X::Y<T*> {};
