// { dg-do assemble  }
// GROUPS passed operators
class a {
public:
    a* operator->() { return this; }
    void p();
};

void a::p() {
  operator->();
}
