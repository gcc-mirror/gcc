// PR c++/15265

enum Relation {equalOp};
template<typename B>
class A {
public:
    static
    bool    Relop(const A&, const A&, Relation);

    friend
    bool    operator==(const A& a1, const A& a2) {
      return Relop(a1, a2, equalOp);
    }
  B* b;
};

int main() {
  A<int> a; a == a;
  return 0;
}


