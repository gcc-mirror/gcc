// PR c++/27506

enum EBorderStyle
  {
    BNATIVE, BHIDDEN
  };
struct BorderValue
{
  enum EBorderStyle style:8;
};
enum EBorderStyle f(const struct BorderValue *border)
{
  return border ?  border->style : BNATIVE;
}
