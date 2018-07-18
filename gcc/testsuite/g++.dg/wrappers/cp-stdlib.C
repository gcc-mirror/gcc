/* At one time this triggered ICEs with location wrapper nodes,
   apparently requiring error-recovery (hence the various syntax
   errors in this file.  */

// { dg-excess-errors "expected to be full of errors, but not an ICE" }

namespace std
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  template<typename _Tp>
    class new_allocator
    {
      typedef _Tp value_type;
    };
}
namespace std
{
  template<typename _Tp>
    using __allocator_base = __gnu_cxx::new_allocator<_Tp>;
}
namespace std __attribute__ ((__visibility__ ("default")))
{
    {
    };
  template<typename _Tp>
    class allocator : public __allocator_base<_Tp>
    {
    };
  template<typename _Alloc>
    struct allocator_traits : __allocator_traits_base
    {
    };
  template<typename _Tp>
    struct allocator_traits<allocator<_Tp>>
    {
      using allocator_type = allocator<_Tp>;
      template<typename _Up>
 using rebind_alloc = allocator<_Up>;
      allocate(allocator_type& __a, size_type __n)
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
template<typename _Alloc, typename = typename _Alloc::value_type>
  struct __alloc_traits
  : std::allocator_traits<_Alloc>
  {
    typedef std::allocator_traits<_Alloc> _Base_type;
    template<typename _Tp>
      struct rebind
      { typedef typename _Base_type::template rebind_alloc<_Tp> other; };
  };
{
    {
 }
 }
    }
    {
 {
    }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
    struct char_traits;
namespace __cxx11 {
  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
    class basic_string;
    }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
namespace __cxx11 {
  template<typename _CharT, typename _Traits, typename _Alloc>
    class basic_string
    {
      typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
 rebind<_CharT>::other _Char_alloc_type;
      typedef __gnu_cxx::__alloc_traits<_Char_alloc_type> _Alloc_traits;
    public:
      {
   {
   }
      }
      operator=(const basic_string& __str)
      {
   {
       {
    {
      const auto __len = __str.size();
      auto __alloc = __str._M_get_allocator();
      auto __ptr = _Alloc_traits::allocate(__alloc, __len + 1);
    }
       }
   }
      }
      {
      }
      size() const noexcept
     }
namespace filesystem
{
  class path
  {
    typedef char value_type;
    typedef std::basic_string<value_type> string_type;
      {
    }
    string_type _M_pathname;
  };
  class directory_entry
  {
    void assign(const filesystem::path& __p) { _M_path = __p; }
    filesystem::path _M_path;
