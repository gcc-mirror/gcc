// PR c++/105353
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wno-psabi" }

typedef unsigned char Simd128U8VectT __attribute__((__vector_size__(16)));

template<int ShuffleIndex>
static inline Simd128U8VectT ShufFunc(Simd128U8VectT vect) noexcept {
    if constexpr(unsigned(ShuffleIndex) >= 16)
        return Simd128U8VectT { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    else if constexpr(ShuffleIndex == 0)
        return vect;
    else
        return __builtin_shufflevector(vect, vect, ShuffleIndex, ShuffleIndex + 1,
            ShuffleIndex + 2, ShuffleIndex + 3, ShuffleIndex + 4, ShuffleIndex + 5,
            ShuffleIndex + 6, ShuffleIndex + 7, ShuffleIndex + 8, ShuffleIndex + 9,
            ShuffleIndex + 10, ShuffleIndex + 11, ShuffleIndex + 12, ShuffleIndex + 13,
            ShuffleIndex + 14, ShuffleIndex + 15);
}

auto func1(Simd128U8VectT vect) noexcept {
    return ShufFunc<5>(vect);
}
