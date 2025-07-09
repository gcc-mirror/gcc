// PR c++/12012
// { dg-do compile { target c++20 } }

template<auto> int x;

int main() {
	x<[](auto) {}>;
}
