// PR c++/80174

typedef unsigned char uint8_t; 

template <typename T>
struct HighestMaxFieldIdx {
  static const uint8_t maxFieldIdx = T::fieldIdx;
};

template <typename This>
struct Outer {

  template <uint8_t _fieldIdx, typename T, T This::*field>
  struct Varint {};


  template <uint8_t _fieldIdx, uint8_t This::*field>
  struct Varint<_fieldIdx, uint8_t, field> {
    static const uint8_t fieldIdx = _fieldIdx;
  };
};

struct Msg {
  uint8_t a;

  static const uint8_t t
  = HighestMaxFieldIdx<Outer<Msg>::Varint<1, uint8_t, &Msg::a> >::maxFieldIdx;
};
