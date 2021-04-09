// PR c++/98687
// { dg-do compile }

extern "C" namespace std {
  double log1p(double);
}
namespace std_fallback {
  template <typename> void log1p();
}
template <typename> struct log1p_impl {
  static int run() {
    using std::log1p;
    using std_fallback::log1p;
    return 0;
  }
};
void log1p() { log1p_impl<int>::run(); }
