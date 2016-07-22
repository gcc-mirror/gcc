// { dg-do run }

using size_t = __SIZE_TYPE__;

inline void* operator new(size_t, void* p) noexcept
{ return p; }

inline void operator delete(void*, void*)
{ }

struct long_t
{
  size_t is_short : 1;
  size_t length   : (__SIZEOF_SIZE_T__ * __CHAR_BIT__ - 1);
  size_t capacity;
  char* pointer;
};

struct short_header
{
  unsigned char is_short : 1;
  unsigned char length   : (__CHAR_BIT__ - 1);
};

struct short_t
{
  short_header h;
  char data[23];
};

union repr_t
{
  long_t      r;
  short_t     s;

  const short_t& short_repr() const
  { return s; }

  const long_t& long_repr() const
  { return r; }

  short_t& short_repr()
  { return s;  }

  long_t& long_repr()
  { return r; }
};

class string
{
public:
  string()
  {
    short_t& s = m_repr.short_repr();
    s.h.is_short = 1;
    s.h.length = 0;
    s.data[0] = '\0';
  }

  string(const char* str)
  {
    size_t length = __builtin_strlen(str);
    if (length + 1 > 23) {
      long_t& l = m_repr.long_repr();
      l.is_short = 0;
      l.length = length;
      l.capacity = length + 1;
      l.pointer = new char[l.capacity];
      __builtin_memcpy(l.pointer, str, length + 1);
    } else {
      short_t& s = m_repr.short_repr();
      s.h.is_short = 1;
      s.h.length = length;
      __builtin_memcpy(s.data, str, length + 1);
    }
  }

  string(string&& other)
    : string{}
  {
    swap_data(other);
  }

  ~string()
  {
    if (!is_short()) {
      delete[] m_repr.long_repr().pointer;
    }
  }

  size_t length() const
  { return is_short() ? short_length() : long_length(); }

private:
  bool is_short() const
  { return m_repr.s.h.is_short != 0; }

  size_t short_length() const
  { return m_repr.short_repr().h.length; }

  size_t long_length() const
  { return m_repr.long_repr().length; }

  void swap_data(string& other)
  {
    if (is_short()) {
      if (other.is_short()) {
        repr_t tmp(m_repr);
        m_repr = other.m_repr;
        other.m_repr = tmp;
      } else {
        short_t short_backup(m_repr.short_repr());
        m_repr.short_repr().~short_t();
        ::new(&m_repr.long_repr()) long_t(other.m_repr.long_repr());
        other.m_repr.long_repr().~long_t();
        ::new(&other.m_repr.short_repr()) short_t(short_backup);
      }
    } else {
      if (other.is_short()) {
        short_t short_backup(other.m_repr.short_repr());
        other.m_repr.short_repr().~short_t();
        ::new(&other.m_repr.long_repr()) long_t(m_repr.long_repr());
        m_repr.long_repr().~long_t();
        ::new(&m_repr.short_repr()) short_t(short_backup);
      } else {
        long_t tmp(m_repr.long_repr());
        m_repr.long_repr() = other.m_repr.long_repr();
        other.m_repr.long_repr() = tmp;
      }
    }
  }

  repr_t m_repr;
};

struct foo
{
  __attribute__((noinline))
  foo(string str)
    : m_str{static_cast<string&&>(str)},
      m_len{m_str.length()}
  { }

  string m_str;
  size_t m_len;
};

int main()
{
  foo f{"the quick brown fox jumps over the lazy dog"};
  if (f.m_len == 0) {
    __builtin_abort();
  }
  return 0;
}
