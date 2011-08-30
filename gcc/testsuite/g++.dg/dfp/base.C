// PR c++/50207
// { dg-do compile }

namespace std
{
  namespace decimal
  {
    template <class _Fmt> struct _FmtTraits;
    class decimal32;
    template <> struct _FmtTraits <decimal32>
    {
      static const long _NumBytes = 4UL;
    };
    template <class _Tr> class _DecBase
    {
      unsigned char _Bytes[_Tr::_NumBytes];
    };
    class decimal32 : public _DecBase <_FmtTraits <decimal32> >	// { dg-error "has base" }
    {
      decimal32 () { }
    };
  }
}
