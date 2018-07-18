// { dg-options -fno-new-ttp-matching }
// { dg-do compile }
namespace mpl {
template <typename, typename = int> struct lambda;
template <typename T3> struct if_ { typedef T3 type; };
template <int> struct arg {
  template <typename U1, typename> struct apply { typedef U1 type; };
};
template <typename> struct begin_impl;
template <typename Sequence> struct begin {
  typedef typename Sequence::tag tag_;
  typedef typename begin_impl<tag_>::template apply<Sequence>::type type;
};
template <typename> struct O1_size_impl;
}
template <long N> struct long_ { static const long value = N; };
namespace mpl {
template <typename Sequence>
struct O1_size
    : O1_size_impl<typename Sequence::tag>::template apply<Sequence> {};
typedef arg<1> _1;
template <typename T> struct protect : T {};
template <typename F> struct apply_wrap1 : F::template apply<int> {};
template <typename F, typename T1, typename T2>
struct apply_wrap2 : F::template apply<T1, T2> {};
template <typename F> struct apply_wrap5 : F::template apply<int> {};
template <typename, typename, typename, typename, typename, typename>
struct resolve_bind_arg;
template <typename T, typename> struct replace_unnamed_arg { typedef T type; };
template <typename F, typename> struct bind1 {
  template <typename> struct apply {
    typedef typename apply_wrap1<F>::type type;
  };
};
template <typename F, typename T1, typename U1, typename U2, typename U3,
          typename U4, typename U5>
struct resolve_bind_arg<bind1<F, T1>, U1, U2, U3, U4, U5> {
  typedef typename apply_wrap5<bind1<F, T1>>::type type;
};
template <typename F, typename, typename T2> struct bind2 {
  template <typename U1, typename U2> struct apply {
    typedef resolve_bind_arg<typename replace_unnamed_arg<T2, arg<1>>::type, U1,
                             U2, int, int, int> t2;
    typedef typename apply_wrap2<F, typename U1::type, typename t2::type>::type
        type;
  };
};
template <typename T> struct quote_impl { typedef T type; };
template <template <typename> class F> struct quote1 {
  template <typename> struct apply : quote_impl<F<int>> {};
};
template <typename T, typename> struct lambda {
  typedef T type;
  typedef arg<1> result_;
};
template <typename, template <typename> class, typename> struct le_result1;
template <template <typename> class F, typename L1>
struct le_result1<int, F, L1> {
  typedef protect<bind1<quote1<F>, typename L1::result_>> type;
};
template <template <typename> class F, typename T1, typename Tag>
struct lambda<F<T1>, Tag> {
  typedef typename le_result1<int, F, lambda<T1>>::type type;
};
template <int, typename, typename, typename> struct iter_fold_impl;
template <typename First, typename State, typename ForwardOp>
struct iter_fold_impl<1, First, State, ForwardOp> {
  typedef typename apply_wrap2<ForwardOp, State, First>::type state;
};
template <typename Sequence, typename State, typename ForwardOp>
struct iter_fold {
  typedef typename iter_fold_impl<O1_size<Sequence>::value,
                                  typename begin<Sequence>::type, State,
                                  ForwardOp>::state type;
};
template <typename> struct deref;
template <typename> struct push_front_impl;
template <typename T> struct l_item {
  typedef int tag;
  typedef l_item type;
  typedef long_<1> size;
  typedef T item;
};
struct l_end {
  typedef int tag;
  typedef l_end type;
};
template <> struct push_front_impl<int> {
  template <typename, typename T> struct apply { typedef l_item<T> type; };
};
template <> struct O1_size_impl<int> {
  template <typename> struct apply : l_item<int>::size {};
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
template <typename> struct list : l_item<int> {};
}
template <typename T> T &cast_storage(void *storage) {
  return *static_cast<T *>(storage);
}
struct symbol;
struct visitation_impl_step {
  typedef symbol type;
};
template <typename Visitor, typename VoidPtrCV, typename T>
void visitation_impl_invoke_impl(int, Visitor visitor, VoidPtrCV storage, T *) {
  visitor.internal_visit(cast_storage<T>(storage), 1);
}
int visitation_impl_invoke_internal_which, visitation_impl_logical_which;
template <typename Visitor, typename VoidPtrCV, typename T,
          typename NoBackupFlag>
void visitation_impl_invoke(Visitor visitor, VoidPtrCV storage, T t,
                            NoBackupFlag, int) {
  visitation_impl_invoke_impl(visitation_impl_invoke_internal_which, visitor,
                              storage, t);
}
template <typename Which, typename step0, typename Visitor, typename VoidPtrCV,
          typename NoBackupFlag>
void visitation_impl(int, Visitor visitor, VoidPtrCV storage, int,
                     NoBackupFlag no_backup_flag, Which, step0 *) {
  switch (visitation_impl_logical_which)
  case 0:
  visitation_impl_invoke(visitor, storage,
                         static_cast<typename step0::type *>(0), no_backup_flag,
                         1);
}
template <long N> struct size_t { static const long value = N; };
struct aligned_storage_imp {};
template <long> class aligned_storage : aligned_storage_imp {
public:
  void *address() { return static_cast<aligned_storage_imp *>(this); }
};
namespace mpl {
template <typename> struct less;
template <typename> struct select_max {
  template <typename OldIterator, typename> struct apply {
    typedef OldIterator type;
  };
};
template <typename Sequence, typename Predicate = less<arg<11>>>
struct max_element : iter_fold<Sequence, typename begin<Sequence>::type,
                               protect<select_max<Predicate>>> {};
template <typename Sequence = int, typename T = int>
struct push_front
    : push_front_impl<typename Sequence::tag>::template apply<Sequence, T> {};
template <> struct push_front<> {
  template <typename T1, typename T2> struct apply : push_front<T1, T2> {};
};
template <typename> struct sizeof_ : size_t<sizeof(int)> {};
template <long, typename, typename, typename, typename>
struct reverse_fold_impl;
template <typename First, typename State, typename BackwardOp,
          typename ForwardOp>
struct reverse_fold_impl<1, First, State, BackwardOp, ForwardOp> {
  typedef First iter0;
  typedef typename apply_wrap2<
      BackwardOp,
      typename apply_wrap2<ForwardOp, State, typename deref<iter0>::type>::type,
      typename deref<iter0>::type>::type state;
};
template <typename Sequence, typename State, typename BackwardOp,
          typename ForwardOp = arg<1>>
struct reverse_fold {
  typedef typename reverse_fold_impl<O1_size<Sequence>::value,
                                     typename begin<Sequence>::type, State,
                                     BackwardOp, ForwardOp>::state type;
};
template <typename> struct inserter {
  typedef mpl::l_end state;
  typedef mpl::push_front<> operation;
};
template <typename Seq, typename Op, typename In>
struct reverse_transform1_impl
    : reverse_fold<Seq, typename In::state,
                   bind2<typename lambda<typename In::operation>::type, _1,
                         bind1<typename lambda<Op>::type, arg<2>>>> {};
template <typename P1, typename P2>
struct transform1
    : if_<reverse_transform1_impl<P1, P2, inserter<push_front<>>>>::type {};
}
template <typename, typename> struct max_value {
  typedef mpl::transform1<mpl::list<symbol>, mpl::sizeof_<mpl::arg<1>>>::type
      transformed_;
  typedef mpl::max_element<transformed_>::type max_it;
  typedef mpl::deref<max_it>::type type;
};
template <typename> struct make_storage {
  typedef max_value<int, mpl::sizeof_<int>>::type max_size;
  typedef aligned_storage<max_size::value> type;
};
struct destroyer {
  template <typename T> void internal_visit(T &operand, int) { operand.~T(); }
};
template <typename, typename...> class variant {
  typedef int internal_types;
  int which_;
  make_storage<internal_types>::type storage_;
public:
  ~variant() {
    destroyer visitor;
    internal_apply_visitor(visitor);
  }
  template <typename Visitor, typename VoidPtrCV>
  void internal_apply_visitor_impl(int internal_which, int, Visitor visitor,
                                   VoidPtrCV storage) {
    visitation_impl(internal_which, visitor, storage, int(), int(),
                    static_cast<int>(0),
                    static_cast<visitation_impl_step *>(0));
  }
  int internal_apply_visitor___trans_tmp_1;
  template <typename Visitor> void internal_apply_visitor(Visitor visitor) {
    internal_apply_visitor_impl(which_, internal_apply_visitor___trans_tmp_1,
                                visitor, storage_.address());
  }
};
struct symbol {
  virtual ~symbol();
};
using Identifier = variant<int>;
struct Fragment {
  virtual void foo() const = 0;
  virtual ~Fragment();
};
struct ProcFrag : Fragment {
  ~ProcFrag() {}
  void foo() const;
  Identifier id;
};
struct Fragments {
  ~Fragments() { delete x; }
  Fragment *x;
} fragments;
