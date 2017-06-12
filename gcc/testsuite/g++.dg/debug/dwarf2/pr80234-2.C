// PR debug/80234
// { dg-do compile }
// { dg-options "-gdwarf-5 -std=c++17" }

struct S
{
  static constexpr const char n = 'S';
  virtual ~S ();
};

constexpr const char S::n;

S::~S()
{
}
