// PR c++/63265
// { dg-do compile { target c++11 } }

#define LSHIFT (sizeof(unsigned int) * __CHAR_BIT__)

template <int lshift>
struct SpuriouslyWarns1 {
    static constexpr unsigned int v = lshift < LSHIFT ? 1U << lshift : 0;
};

static_assert(SpuriouslyWarns1<LSHIFT>::v == 0, "Impossible occurred");

template <int lshift>
struct SpuriouslyWarns2 {
    static constexpr bool okay = lshift < LSHIFT;
    static constexpr unsigned int v = okay ? 1U << lshift : 0;
};

static_assert(SpuriouslyWarns2<LSHIFT>::v == 0, "Impossible occurred");
