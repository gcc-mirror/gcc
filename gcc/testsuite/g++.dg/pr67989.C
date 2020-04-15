/* { dg-do compile } */
/* { dg-options "-std=c++11 -O2 -Wno-return-type" } */

__extension__ typedef unsigned long long int uint64_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  typedef enum memory_order
  {
    memory_order_seq_cst
  } memory_order;
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp > struct atomic
  {
    static constexpr int _S_min_alignment
      = (sizeof (_Tp) & (sizeof (_Tp) - 1)) || sizeof (_Tp) > 16
      ? 0 : sizeof (_Tp);
    static constexpr int _S_alignment
      = _S_min_alignment > alignof (_Tp) ? _S_min_alignment : alignof (_Tp);
      alignas (_S_alignment) _Tp _M_i;
    operator  _Tp () const noexcept
    {
      return load ();
    }
    _Tp load (memory_order __m = memory_order_seq_cst) const noexcept
    {
      _Tp tmp;
        __atomic_load (&_M_i, &tmp, __m);
    }
  };
}

namespace lldb_private
{
  namespace imp
  {
  }
  class Address;
}
namespace lldb
{
  typedef uint64_t addr_t;
  class SBSection
  {
  };
  class SBAddress
  {
    void SetAddress (lldb::SBSection section, lldb::addr_t offset);
      lldb_private::Address & ref ();
  };
}
namespace lldb_private
{
  class Address
  {
  public:
    const Address & SetOffset (lldb::addr_t offset)
    {
      bool changed = m_offset != offset;
    }
    std::atomic < lldb::addr_t > m_offset;
  };
}

using namespace lldb;
using namespace lldb_private;
void
SBAddress::SetAddress (lldb::SBSection section, lldb::addr_t offset)
{
  Address & addr = ref ();
  addr.SetOffset (offset);
}
