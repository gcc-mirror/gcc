// PR c++/27270
// { dg-options "" }

template<typename Entry>
struct Array {
  Entry *array[32];
  Array () :
    array ( (Entry*[1]) { 0, 0 } ) // { dg-error "initializers|incompatible" }
  {}
};

Array<void*> a;
