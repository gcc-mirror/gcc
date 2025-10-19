// Check that we do not get a crash
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce" }


struct Swapper {
  static unsigned int swapBytes();
  static bool isFinished();
};


struct Capacity {
    Capacity(int capacity);
    bool operator<( int rhs) const;
    bool operator>=(int rhs) const;
};

template <class CAPACITY,  class SWAPPER>
struct Utf32ToUtf8Translator {

    CAPACITY d_capacity;

    Utf32ToUtf8Translator();

    static void translate();
};

template <class CAPACITY,class SWAPPER>
void Utf32ToUtf8Translator<CAPACITY, SWAPPER>::translate()
{
    Utf32ToUtf8Translator<CAPACITY, SWAPPER> translator;

    unsigned int uc =0;
    while (!SWAPPER::isFinished()) {
        uc = SWAPPER::swapBytes();
        if (0 != uc) {
        }
    }
    contract_assert( translator.d_capacity >= 1 );

}

void func()
{
    typedef Utf32ToUtf8Translator<Capacity,
                                  Swapper> SwapTranslator;

    SwapTranslator::translate();
}
