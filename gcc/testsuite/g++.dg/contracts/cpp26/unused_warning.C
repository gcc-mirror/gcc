// check that we do not get unused warnings for contract check function parameters
// { dg-do compile }
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=enforce  -Wall -Wextra" }

struct TimeInterval{
  int i = 4;
  void addInterval(int i, int j);
};

TimeInterval operator-(const TimeInterval& lhs, const TimeInterval& rhs)
    pre(2 != rhs.i);

inline
TimeInterval operator-(const TimeInterval& lhs,
                                   const TimeInterval& rhs)

{
    TimeInterval result(lhs);
    result.addInterval(-rhs.i, -rhs.i);
    return result;
}

int main(int, char**) {

}
