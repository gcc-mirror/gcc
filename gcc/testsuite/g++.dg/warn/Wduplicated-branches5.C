// PR c++/83924
// { dg-do compile { target c++11 } }
// { dg-options "-Wduplicated-branches" }

class GenVectorS {};

template<int N>
using  VectorS = GenVectorS;

template<int n, int m>
void runB(const VectorS<(n > m ? n : m)>)
{}

void runA() {
	runB<1, 1>(VectorS<1>{});
}
