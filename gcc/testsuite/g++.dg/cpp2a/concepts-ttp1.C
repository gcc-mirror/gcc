// PR c++/95371
// { dg-do compile { target c++20 } }

template <typename...>
struct configuration  {
    template <template <typename...> typename query_t>
    static constexpr bool exists() { return true; }

    template <template <typename...> typename query_t>
    void remove() requires(exists<query_t>());
};

int main() {
    configuration<> cfg{};
    cfg.remove<configuration>();
}
