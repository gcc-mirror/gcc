namespace NS {
class X {};
typedef X Y;
}

struct Base : virtual public NS::Y {
  Base() : NS::Y() {}
};
