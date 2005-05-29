// PR c++/21340

struct Base{};
struct Iterator : virtual Base {};
bool operator==(const Iterator&, const Iterator&);
struct IteratorI : Iterator {};
struct Obj
{
  bool operator==(const Obj&) const;
};
template <int>bool dummy()
{
  Obj lhs, rhs;
  return lhs == rhs;
}
int
main(int argc, char** argv)
{
  IteratorI* it2 = new IteratorI();
}
