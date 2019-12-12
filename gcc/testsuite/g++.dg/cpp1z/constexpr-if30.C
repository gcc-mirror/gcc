// { dg-do compile { target c++17 } }

auto fn = [](auto i) {
    if constexpr (sizeof(i) == 1)
        return fn(123);		// { dg-error "auto" }
};

int main() {
    fn('!');
}
