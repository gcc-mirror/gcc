// PR debug/80234
// { dg-do compile { target c++17 } }
// { dg-options "-gdwarf-5" }

struct S
{
  static constexpr const char n = 'S';
  virtual ~S ();
};

constexpr const char S::n;

S::~S()
{
}
