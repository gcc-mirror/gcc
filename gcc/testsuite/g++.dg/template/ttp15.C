struct Dense {
  static const unsigned int dim = 1;
};

template <template <typename> class View,
	  typename Block>
void operator+(float, View<Block> const&);

template <typename Block,
	  unsigned int Dim = Block::dim>
struct Lvalue_proxy {
  operator float() const;
};

void
test_1d (void)
{
  Lvalue_proxy<Dense> p;
  float b;
  b + p;
}
