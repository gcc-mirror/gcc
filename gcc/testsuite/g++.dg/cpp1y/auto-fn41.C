// PR c++/80873
// { dg-do compile { target c++14 } }

struct S {};

auto overloaded(S &);

template <typename T>
int overloaded(T &) {
    return 0;
}

template <typename T>
auto returns_lambda(T &param) {
	return [&] {
		overloaded(param);  // { dg-error "before deduction" }
	};
}

int main() {
	S s;
	returns_lambda(s);
}
