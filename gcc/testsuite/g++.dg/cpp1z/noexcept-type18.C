// { dg-do compile { target c++17 } }

template<typename T>
struct S;

template<bool IsNoexcept>
struct S<void(*)() noexcept(IsNoexcept)> {
	S() {}
};

void f() {}

int main() {
	S<decltype(&f)> {};
}
