// PR 99389 failed to stream class specialization
// { dg-module-do link }
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }
template<typename _CharT>
class basic_string_view
{
public:
  basic_string_view(const _CharT* __str) noexcept
  {}
  bool
    empty() const noexcept
  { return !_M_len; }
  
private:
  unsigned _M_len;
};

using string_view = basic_string_view<char>;

