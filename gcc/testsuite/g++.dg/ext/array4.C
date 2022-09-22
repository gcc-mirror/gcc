// PR c++/101029
// { dg-do compile { target c++11 } }
// { dg-options "" } allow [0]

template <int __v> struct integral_constant {
  static constexpr int value = __v;
  typedef int value_type;
  constexpr operator value_type() { return __v; }
};
template <bool __v> using __bool_constant = integral_constant<__v>;
template <bool, typename> struct conditional;
template <typename...> struct __and_;
template <typename _B1, typename _B2, typename _B3, typename... _Bn>
struct __and_<_B1, _B2, _B3, _Bn...> : conditional<_B1::value, _B1> {};
template <typename _Tp>
constexpr integral_constant<true> __is_complete_or_unbounded(_Tp) {
  return {};
}
template <typename _Tp>
struct is_default_constructible : integral_constant<false> {
  static_assert(__is_complete_or_unbounded(_Tp{}), "");
};
template <typename _Tp, typename _Up>
struct is_same : integral_constant<__is_same_as(_Tp, _Up)> {};
template <bool> struct enable_if;
template <typename _Iffalse> struct conditional<false, _Iffalse> {
  typedef _Iffalse type;
};
struct pair {
  template <typename _U1 = int, typename _U2 = int,
            typename enable_if<__and_<is_default_constructible<_U1>, _U2,
                                      int>::valuebool>::type>
  pair();
};

class BucketLogger;
struct __shared_ptr_access {
  using element_type = BucketLogger;
  element_type *operator->();
};
struct DcpProducer {
  __shared_ptr_access logger;
  void bufferAcknowledgement();
};
struct atomic {
  atomic(long);
};
inline namespace v7 {
template <bool B, class, class F>
using conditional_t = typename conditional<B, F>::type;
template <typename> struct basic_string_view { basic_string_view(int, int); };
template <typename, typename> struct formatter;
template <typename, typename>
using has_formatter =
    __bool_constant<__is_constructible(void)>;
struct fallback_formatter;
template <typename Context> struct custom_value {
  using parse_context = typename Context::parse_context_type;
  void (*format)(const void *, parse_context &, Context &);
};
template <typename Context> struct value {
  float float_value;
  custom_value<Context> custom;
  template <typename T> value(T) {
    custom.format =
        format_custom_arg<T, conditional_t<has_formatter<T, Context>::value,
                                           typename Context::formatter_type<T>,
                                           fallback_formatter>>;
  }
  template <typename, typename Formatter>
  static void format_custom_arg(const void *arg,
                                typename Context::parse_context_type &,
                                Context &ctx) {
    Formatter f;
    f.format(*static_cast<const int *>(arg), ctx);
  }
};
enum { max_packed_args };
template <typename Context> struct basic_format_arg { value<Context> value_; };
template <typename Visitor, typename Context>
void visit_format_arg(Visitor vis, Context arg) {
  vis(arg.value_.float_value);
}
template <typename Context, typename T> basic_format_arg<Context> make_arg(T);
struct basic_format_context {
  using char_type = int;
  using parse_context_type = int;
  template <typename T> using formatter_type = formatter<T, char_type>;
};
struct format_arg_store {
  using value_type = conditional_t<max_packed_args, basic_format_context,
                                   basic_format_arg<basic_format_context>>;
  value_type data_;
};
template <typename... Args, typename S>
auto make_args_checked(S, Args... args) -> format_arg_store {
  return {args...};
}
struct basic_format_specs {};
template <typename Char, typename OutputIt, typename T>
void write(OutputIt, T, Char) {
  if (is_same<T, float>())
    ;
}
struct arg_formatter_base {
  using iterator = int;
  using format_specs = basic_format_specs;
  iterator out_;
  template <typename T> void operator()(T value) {
    auto specs = format_specs();
    write(out_, value, specs);
  }
};
struct arg_formatter : arg_formatter_base {
  using context_type = basic_format_context;
  arg_formatter(context_type, int *, format_specs *);
};
template <typename T, typename> struct formatter {
  template <typename FormatContext> void format(T val, FormatContext ctx) {
    using af = arg_formatter;
    basic_format_arg<FormatContext> __trans_tmp_2 = make_arg<FormatContext>(val);
    visit_format_arg(af(ctx, nullptr, &specs_), __trans_tmp_2);
  }
  basic_format_specs specs_;
};
} // namespace v7
namespace level {
enum level_enum { warn };
}
struct BucketLogger {
  template <typename S, typename... Args>
  void log(level::level_enum, const S &, Args &&...);
  template <typename... Args> void warn(const char *, const Args &...);
};
namespace v7 {
struct fallback_formatter : formatter<basic_string_view<int>, int> {
  template <typename OutputIt> void format(int, OutputIt ctx) {
    basic_string_view<int> str(0, 0);
    formatter::format(str, ctx);
  }
};
} // namespace v7
template <typename S, typename... Args>
void BucketLogger::log(level::level_enum, const S &fmt, Args &&...args) {
  make_args_checked(fmt, args...);
}
template <typename... Args>
void BucketLogger::warn(const char *fmt, const Args &...args) {
  log(level::warn, fmt, args...);
}
template <class KeyT> struct AtomicHashArray {
  static void create();
  atomic isFull_;
  atomic numErases_;
  pair cells_[0];
  AtomicHashArray(int, KeyT, KeyT, KeyT, double, unsigned);
};
template <class KeyT>
AtomicHashArray<KeyT>::AtomicHashArray(int, KeyT, KeyT, KeyT, double, unsigned)
    : isFull_(0), numErases_(0) {}
template <class KeyT> void AtomicHashArray<KeyT>::create() {
  int c_4, capacity;
  double c_3;
  char c_2, c_1, c_0;
  AtomicHashArray(capacity, c_0, c_1, c_2, c_3, c_4);
}
int bufferAcknowledgement_vbucket;
void DcpProducer::bufferAcknowledgement() {
  logger->warn("", bufferAcknowledgement_vbucket);
}
void (*makeStreamsMap_p)() = AtomicHashArray<char>::create;
