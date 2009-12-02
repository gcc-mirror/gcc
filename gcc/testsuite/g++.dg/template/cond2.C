// PR c++/11962
// { dg-options "" }

template<int X> class c;

template<int X, int Y> int test(c<X ? : Y>&);

void test(c<2>*c2) {
	test<0, 2>(*c2); // { dg-error "omitted" }
}
