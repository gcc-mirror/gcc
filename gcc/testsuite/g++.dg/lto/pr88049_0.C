// PR c++/88049
// { dg-lto-do link }
// { dg-lto-options {{ -flto -O2 -w }} }
// { dg-require-effective-target lto_incremental }
// { dg-extra-ld-options -r }

template <typename> class a;
class b {};
template <typename e> a<e> d(char);
template <typename> class a : public b {
public:
  virtual ~a();
};
namespace {
  class f;
  b c = d<f>(int());
} // namespace
