// PR c++/104235

template <class M>
struct L: M {
  using M::a;
  void a();
  void p() { this->template a<>(); }
};
