// PR c++/86969
// { dg-do compile { target c++17 } }

auto compose = [](auto... fs) {
    if constexpr (sizeof...(fs) == 0) {
        return [](auto x) { return x; };
    } else {
        auto fn = [](auto self, auto f, auto... fs) {
            if constexpr (sizeof...(fs) == 0) return f;
            else return [=](auto x) { 
                return f(self(self, fs...)(x));
            };
        };
        return fn(fn, fs...);
    }
};

static_assert(compose(
        [](auto x) { return x * 3; },
        [](auto x) { return x + 1; },
        [](auto x) { return x / 2; }
    )(6) == 12);
