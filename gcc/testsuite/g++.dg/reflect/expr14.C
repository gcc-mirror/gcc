// PR c++/123081
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<typename T>
constexpr auto refl = ^^T;

struct {
	template<typename T>
	requires(requires { typename[:refl<T>:]; })
	void f(T) {}
} a;

int main() {
	a.f([] {});
}
