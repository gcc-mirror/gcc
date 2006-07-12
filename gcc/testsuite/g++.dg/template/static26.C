// PR c++/28016
// { dg-final { scan-assembler-not "computed" } }

template<class T1, class T2>
struct scalar_divides_assign {
  static const bool computed ;
};

template<class T1, class T2>
const bool scalar_divides_assign<T1,T2>::computed = true;
