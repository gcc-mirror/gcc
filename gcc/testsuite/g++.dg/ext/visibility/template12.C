// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-not-hidden "_ZN1aI1bE1cE" } }

template <class T> class __attribute__((visibility("default"))) a
{
public:
  /* A */ static int c;
};

class __attribute__((visibility("default"))) b : a <b> {};

template<> /* B */ int a<b>::c = 0;
