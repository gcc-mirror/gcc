// { dg-do compile }
// { dg-additional-options "-std=c++11" }

template <typename> struct remove_reference;
template <typename _Tp> struct remove_reference<_Tp &> { typedef _Tp type; };
template <typename _Tp> typename remove_reference<_Tp>::type move(_Tp &&p1) {
  return static_cast<typename remove_reference<_Tp>::type &&>(p1);
}
void *operator new(__SIZE_TYPE__, void *p2) { return p2; }
struct Trans_NS__v1_GenericTlv {
  virtual int getMinimumValueLength();
  virtual unsigned long getValueLength() const;
};
struct IPv4NeighborAddressSubTlv;
struct Trans_NS__v1_GenericTlvBase : Trans_NS__v1_GenericTlv {
  virtual bool operator==(const IPv4NeighborAddressSubTlv &) const;
};
struct Trans_NS__v1_GenericUnsupportedTlv;
template <typename> struct backup_holder {
  Trans_NS__v1_GenericUnsupportedTlv *backup_;
  Trans_NS__v1_GenericUnsupportedTlv &get() { return *backup_; }
};
template <typename> struct make_reference_content {
  typedef IPv4NeighborAddressSubTlv type;
};
template <typename> struct unwrap_recursive {
  typedef IPv4NeighborAddressSubTlv type;
};
template <typename> struct begin_impl;
template <typename Sequence> struct begin {
  typedef typename Sequence::tag tag_;
  typedef typename begin_impl<tag_>::template apply<Sequence>::type type;
};
struct long_ {
  static const int value = 0;
};
template <typename> struct O1_size_impl;
template <typename Sequence>
struct O1_size
    : O1_size_impl<typename Sequence::tag>::template apply<Sequence> {};
template <typename F, typename T2>
struct apply_wrap2 : F::template apply<int, T2> {};
template <int, typename, typename> struct iter_fold_impl;
template <typename First, typename ForwardOp>
struct iter_fold_impl<0, First, ForwardOp> {
  typedef typename apply_wrap2<ForwardOp, First>::type state;
};
template <typename Sequence, typename ForwardOp> struct iter_fold {
  typedef
      typename iter_fold_impl<O1_size<Sequence>::value,
                              typename begin<Sequence>::type, ForwardOp>::state
          type;
};
template <typename> struct deref;
template <typename T1> struct pair { typedef T1 first; };
struct make_initializer_node {
  template <typename, typename Iterator> struct apply {
    struct initializer_node {
      typedef typename deref<Iterator>::type recursive_enabled_T;
      static int
      initialize(void *p1,
                 typename unwrap_recursive<recursive_enabled_T>::type) {
        new (p1) typename make_reference_content<recursive_enabled_T>::type;
      }
    };
    typedef pair<initializer_node> type;
  };
};
struct l_item {
  typedef int tag;
  typedef l_item type;
  typedef long_ size;
  typedef int item;
};
template <> struct O1_size_impl<int> {
  template <typename List> struct apply : List::size {};
};
template <typename> struct l_iter;
template <typename Node> struct deref<l_iter<Node>> {
  typedef typename Node::item type;
};
template <> struct begin_impl<int> {
  template <typename List> struct apply {
    typedef l_iter<typename List::type> type;
  };
};
template <typename, typename, typename, typename, typename, typename, typename>
struct list : l_item {};
template <typename... T> struct make_variant_list { typedef list<T...> type; };
template <typename T> T cast_storage(void *p1) { return *static_cast<T *>(p1); }
struct visitation_impl_step {
  typedef Trans_NS__v1_GenericUnsupportedTlv type;
};
template <typename Visitor, typename VoidPtrCV, typename T>
void visitation_impl_invoke_impl(Visitor p1, VoidPtrCV p2, T *) {
  backup_holder<Trans_NS__v1_GenericUnsupportedTlv> __trans_tmp_8 =
      cast_storage<backup_holder<T>>(p2);
  p1.internal_visit(__trans_tmp_8, 0);
}
template <typename Visitor, typename VoidPtrCV, typename T,
          typename NoBackupFlag>
void visitation_impl_invoke(Visitor p1, VoidPtrCV p2, T p3, NoBackupFlag) {
  visitation_impl_invoke_impl(p1, p2, p3);
}
template <typename Which, typename step0, typename Visitor, typename VoidPtrCV,
          typename NoBackupFlag>
void visitation_impl(Visitor p1, VoidPtrCV p2, NoBackupFlag, Which, step0 *) {
  visitation_impl_invoke(p1, p2, static_cast<typename step0::type *>(0), 0);
}
struct move_into {
  move_into(void *);
  template <typename T> void internal_visit(backup_holder<T> p1, int) {
    T __trans_tmp_2 = p1.get();
    new (0) T(__trans_tmp_2);
  }
};
template <typename, typename... TN> struct variant {
  struct initializer : iter_fold<typename make_variant_list<int, TN...>::type,
                                 make_initializer_node>::type::first {};
  template <typename T> void convert_construct(T p1, int) {
    void *__trans_tmp_9 = this;
    initializer::initialize(__trans_tmp_9, p1);
  }
  template <typename T> variant(T p1) { convert_construct(p1, 0); }
  variant(variant &&p1) {
    move_into visitor(0);
    p1.internal_apply_visitor(visitor);
  }
  template <typename Visitor> void internal_apply_visitor(Visitor p1) {
    void *__trans_tmp_10 = this;
    visitation_impl(p1, __trans_tmp_10, 0, 0,
                    static_cast<visitation_impl_step *>(0));
  }
};
template <class...> struct generic_element_tlvs;
template <typename TlvConfig, class UnsupportedTlvClass, class TlvF,
          class... TlvR>
struct generic_element_tlvs<TlvConfig, UnsupportedTlvClass, TlvF, TlvR...> {
  typedef variant<UnsupportedTlvClass, TlvF, TlvR...> variant_type;
};
template <typename, typename> struct Trans_NS__v1_GenericTlvContainer {
  template <class TlvClass> void addTlv(const TlvClass &);
};
template <typename TlvConfig, typename ElementTlvs>
template <class TlvClass>
void Trans_NS__v1_GenericTlvContainer<TlvConfig, ElementTlvs>::addTlv(
    const TlvClass &p1) {
  typename ElementTlvs::variant_type wrap(p1);
  move(wrap);
}
template <typename ElementTlvs>
struct Trans_NS__v1_GenericContainerEntryBase
    : Trans_NS__v1_GenericTlvContainer<int, ElementTlvs> {};
template <class>
struct Trans_NS__v1_GenericFixedLengthTlvBase : Trans_NS__v1_GenericTlvBase {
  unsigned long getValueLength() const;
};
struct Trans_NS__v1_GenericUnsupportedTlv : Trans_NS__v1_GenericTlv {
  long getHeaderLengthconst;
};
using isis_tlv_config = int;
template <class... TlvClasses>
using isis_element_tlvs =
    generic_element_tlvs<isis_tlv_config, Trans_NS__v1_GenericUnsupportedTlv,
                         TlvClasses...>;
template <int, class, typename ElementTlvs>
using ContainerEntryBase = Trans_NS__v1_GenericContainerEntryBase<ElementTlvs>;
template <int, class ImplClass, int>
using FixedLengthTlvBase = Trans_NS__v1_GenericFixedLengthTlvBase<ImplClass>;
struct IPv4NeighborAddressSubTlv
    : FixedLengthTlvBase<0, IPv4NeighborAddressSubTlv, 0> {
  bool operator==(const IPv4NeighborAddressSubTlv &) const;
};
void test() {
  ContainerEntryBase<
      0, int,
      isis_element_tlvs<
          FixedLengthTlvBase<0, int, 0>, FixedLengthTlvBase<0, int, 0>,
          IPv4NeighborAddressSubTlv, FixedLengthTlvBase<0, int, 0>,
          FixedLengthTlvBase<0, int, 0>, FixedLengthTlvBase<0, int, 0>>>
      isEntry;
  IPv4NeighborAddressSubTlv nbAddressSubTlv;
  isEntry.addTlv(nbAddressSubTlv);
}
