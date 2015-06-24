// PR c++/65750
// { dg-do compile { target c++11 } }

template<typename T> struct F { };

class a
{
  virtual auto f( F< void () > ) -> void;
  virtual auto g( F< auto () -> void > ) -> void;
};

auto main() -> int { }
