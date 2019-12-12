// PR c++/67114
// { dg-do compile { target c++17 } }

typedef unsigned uint32_t;
class A {
public:
  void operator==(A);
  operator uint32_t() const;
  operator int() const;
};
class BluetoothNamedValue {
  bool operator==(const BluetoothNamedValue &) const;
  A value() const;
};
auto BluetoothNamedValue::
operator==(const BluetoothNamedValue &) const -> bool {
  value() == value();
  return true;
}
