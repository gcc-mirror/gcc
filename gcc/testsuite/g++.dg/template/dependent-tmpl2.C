// PR c++/100078
// { dg-do compile { target c++11 } }

template <bool> struct enable_if;
template <typename Data> struct HashMapBucket {
  template <typename T = Data>
  static typename enable_if<T ::value>::type selectStructure() {
    selectStructure();
  }
};
