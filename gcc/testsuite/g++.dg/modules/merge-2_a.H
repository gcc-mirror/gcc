// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _CharT> class istreambuf_iterator;

void move(char __t);
void move(istreambuf_iterator<char> &__u);

template<typename _Tp> struct allocator {};

template<typename _Alloc> struct __alloc_traits
{
  static void _S_on_swap(_Alloc& __b)
  {
    move (__b);
  }

  typedef allocator<char> other;
};

template<typename _CharT> class basic_string
{
  typedef __alloc_traits<char>::other _Char_alloc_type;
};

template<typename _CharT> class istreambuf_iterator
{
  void frob (const basic_string<char>& __s);
};
