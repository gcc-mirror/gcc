// PR c++/117775
// Check that we don't ICE and have sizeof(void)==1 under -Wno-pointer-arith
// { dg-do run { target c++20 } }
// { dg-additional-options "-Wno-pointer-arith" }

int main() {
  struct why :
    decltype( [](auto) {
		return sizeof(void);
	      })
  {} x;
  return 1 - x.operator()(0);
}
