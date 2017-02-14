// PR c++/78765
// { dg-do compile { target c++11 } }

// ICE with failed constexpr object and member fn call

struct ValueType {
  constexpr operator int() const {return field;}
  int field;
};

static constexpr ValueType var = 0; // { dg-error "conversion" }

template <int> class ValueTypeInfo;

ValueTypeInfo<var> x;
