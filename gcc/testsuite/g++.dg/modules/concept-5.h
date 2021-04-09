template<typename T>
requires (sizeof (T) == 1)
constexpr int f1 (T x) { return 1; }

template<typename T>
requires (sizeof (T) != 1)
constexpr int f1 (T x) { return 0; }
