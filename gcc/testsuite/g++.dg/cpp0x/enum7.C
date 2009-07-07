// PR c++/37816
// { dg-options "-std=c++0x" }

class A
{
  enum class Color { Red, Orange, Yellow, Green, Blue, Violet };
  enum class Alert { Green, Yellow, Red };
  static const Color x = Red;	// { dg-error "" }
  static const Color y = Color::Red;
  static const Alert z = Alert::Red;
};
