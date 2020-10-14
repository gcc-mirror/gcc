// { dg-do compile }
// { dg-require-effective-target c++11 }

inline namespace __cxx11 {}
typedef int size_t;
class MessageAuthenticationCode;
class __uniq_ptr_impl {
  struct _Ptr {
    using type = MessageAuthenticationCode *;
  };
public:
  using pointer = _Ptr::type;
};
class unique_ptr {
public:
  using pointer = __uniq_ptr_impl::pointer;
  unique_ptr(pointer);
};
namespace __cxx11 {
class basic_string {
public:
  basic_string(char *);
  ~basic_string();
};
} // namespace __cxx11
class MessageAuthenticationCode {};
class SCAN_Name {
public:
  SCAN_Name(basic_string);
  size_t arg_as_integer();
};
class SipHash : public MessageAuthenticationCode {
public:
  SipHash(size_t c, size_t d) : m_C(c), m_D(d) {}
  size_t m_C, m_D;
};
void create(basic_string algo_spec, char *s) {
  basic_string provider = s;
  SCAN_Name req(algo_spec);
  unique_ptr(new SipHash(req.arg_as_integer(), req.arg_as_integer()));
}
