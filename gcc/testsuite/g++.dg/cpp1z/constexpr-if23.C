// PR c++/85842
// { dg-do compile { target c++17 } }

template<class T>
auto f = [](auto&& arg) -> T* {
    if constexpr (sizeof(arg) == 1) {
        return nullptr;
    } else {
        return static_cast<T*>(&arg);
    }
};

auto p = f<int>(0);
