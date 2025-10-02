// PR c++/119048
// { dg-do compile { target c++23 } }

int main() {
	[] {}, [](...) static {};
}
