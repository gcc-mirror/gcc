// { dg-do assemble  }

template<class T>
struct X_two {
  template <class T2> T2 conv_compare_ge(T2 test) {
    T2 tmp_value = T2 (0);
    return (tmp_value > test ? tmp_value : test);
  }
};

template int X_two<double>::conv_compare_ge(int);

