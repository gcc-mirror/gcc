// PR c++/25342

template < typename eval >
struct tpl_seq_search {
  typedef typename eval::enum_type  Enum;
  template < Enum first, Enum last >
  struct range {
  };
  template < Enum val >
  struct range<val,val> {
  };
};
struct xxx {
  typedef int enum_type;
  tpl_seq_search<xxx>::range<0, 1> a;
};
