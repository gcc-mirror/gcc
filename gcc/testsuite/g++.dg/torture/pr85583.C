/* { dg-do link } */
class b {
public:
  virtual ~b();
};
template <typename> class c : b {};
class B {
  c<char> d;
};
extern template class c<char>;
int
main(void) { B a; return 0; }

