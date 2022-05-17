// PR c++/64679
// { dg-do compile { target c++11 } }

struct F {
  F(int, int);
};

void
g ()
{
  int x = 42;
  
  F v1(int(x), decltype(x)(42));

  F f1(int(i), decltype(i) j = 42);
  F f2(int(i), decltype(i) j);
  F f3(int(i), decltype(i)(j));	// { dg-warning "function declaration" }
  F f4(int(i), decltype(i)(j) = 42); // { dg-warning "function declaration" }
  F f5(int (i), bool b = true, decltype(i) j = 42);
  F f6(int(i), decltype(x)(x)); // { dg-warning "function declaration" }
}
