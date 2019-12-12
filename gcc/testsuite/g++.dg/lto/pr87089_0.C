// { dg-lto-do link }
// { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }
namespace itpp {
template <class a> void b(a *c) { c[0].~a(); }
class CFix;
template <class> class d {
  void e(const char *);
  CFix *data;
};
class CFix {
public:
  virtual ~CFix();
};
template <> void d<int>::e(const char *) { b(data); }
} // namespace itpp

int
main (void)
{
  return 0;
}
