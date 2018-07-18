// PR c++/80873
// { dg-do compile { target c++14 } }

struct Buffer {};

auto parse(Buffer b);
template <typename T> void parse(T target);

template <typename T>
auto field(T target) {
	return [&] {
		parse(target);
	};
}

template <typename T>
void parse(T target) {}

auto parse(Buffer b) {
	field(0);
}
