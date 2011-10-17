// PR c++/44524

template<typename, typename>
struct map
{
  bool empty();
};

int bar(map<int, float> *X) {
  return X.empty();  // { dg-error "which is of pointer type 'map" }
}
