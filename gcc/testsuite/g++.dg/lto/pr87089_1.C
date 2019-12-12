namespace itpp {
enum a { b };
class CFix {
public:
  virtual ~CFix();
};
template <a = b> class c : CFix {
  ~c() {}
};
template class c<>;
} // namespace itpp

