// Origin PR c++/45200
// { dg-do compile }

template<typename T>
struct remove_reference
{
  typedef T type;
};

template<typename TestType>
struct forward_as_lref
{
};

template<typename Seq, typename N>
struct apply1
{
  typedef typename remove_reference<Seq>::type seq;
  typedef forward_as_lref<typename seq::seq_type> type; //#0
};

template<typename Seq>
struct apply
{
  typedef forward_as_lref<typename remove_reference<Seq>::type::seq_type> type; //#1
};

struct reverse_view
{
  typedef int seq_type;
};

int
main()
{
  apply<reverse_view >::type a2;
}
