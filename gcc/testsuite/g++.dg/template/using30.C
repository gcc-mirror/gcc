// PR c++/105006

template<class eT>
class Row {
  using eT::operator();
  void operator()();
  class fixed;
};

template<class eT>
class Row<eT>::fixed : Row {
  using Row::operator();
};
