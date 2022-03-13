// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;
# 5 __FILE__ 1
class Pooh;
class Piglet;
# 8 "" 2

export module std; // might happen, you can't say it won't!
// { dg-module-cmi std }

namespace std {
export template<typename T> class allocator {
// just for testing, not real!
void M (T *);
template <typename U> U *N (T *);
};

template<typename T> void allocator<T>::M (T *) {}
template<typename T> template<typename U> U *allocator<T>::N (T *) {
return nullptr;
}

template void allocator<int>::M (int *);
template float *allocator<int>::N<float> (int *);
}

template void std::allocator<Pooh>::M (Pooh *);
template Piglet *std::allocator<Pooh>::N<Piglet> (Pooh *);

// { dg-final { scan-assembler {_ZNStW3std9allocatorIiE1MEPi:} } }
// { dg-final { scan-assembler {_ZNStW3std9allocatorIiE1NIfEEPT_Pi:} } }
// { dg-final { scan-assembler {_ZNStW3std9allocatorI4PoohE1MEPS1_:} } }
// { dg-final { scan-assembler {_ZNStW3std9allocatorI4PoohE1NI6PigletEEPT_PS1_:} } }
