// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=c++11 -O3 -msse2 -mno-avx -fno-exceptions -fno-rtti -fdump-rtl-final" }

typedef unsigned int size_type;

#define _GLIBCXX_BITSET_BITS_PER_WORD  (__CHAR_BIT__ * __SIZEOF_INT__)
#define _GLIBCXX_BITSET_WORDS(__n) \
  ((__n) / _GLIBCXX_BITSET_BITS_PER_WORD + \
   ((__n) % _GLIBCXX_BITSET_BITS_PER_WORD == 0 ? 0 : 1))

namespace std
{
  template<size_type _Nw>
    struct _Base_bitset
    {
      typedef unsigned int _WordT;
      _WordT 		_M_w[_Nw];

      _WordT&
      _M_hiword()
      { return _M_w[_Nw - 1]; }

      void
      _M_do_and(const _Base_bitset<_Nw>& __x)
      {
	for (size_type __i = 0; __i < _Nw; __i++)
	  _M_w[__i] &= __x._M_w[__i];
      }

      void
      _M_do_flip()
      {
	for (size_type __i = 0; __i < _Nw; __i++)
	  _M_w[__i] = ~_M_w[__i];
      }

      bool
      _M_is_equal(const _Base_bitset<_Nw>& __x) const
      {
	for (size_type __i = 0; __i < _Nw; ++__i)
	  if (_M_w[__i] != __x._M_w[__i])
	    return false;
	return true;
      }

      bool
      _M_is_any() const
      {
	for (size_type __i = 0; __i < _Nw; __i++)
	  if (_M_w[__i] != static_cast<_WordT>(0))
	    return true;
	return false;
      }
    };

  template<size_type _Extrabits>
    struct _Sanitize
    {
      typedef unsigned int _WordT;

      static void
      _S_do_sanitize(_WordT& __val)
      { __val &= ~((~static_cast<_WordT>(0)) << _Extrabits); }
    };

  template<size_type _Nb>
    class bitset
    : private _Base_bitset<_GLIBCXX_BITSET_WORDS(_Nb)>
    {
    private:
      typedef _Base_bitset<_GLIBCXX_BITSET_WORDS(_Nb)> _Base;
      typedef unsigned int _WordT;

      void
      _M_do_sanitize()
      {
	typedef _Sanitize<_Nb % _GLIBCXX_BITSET_BITS_PER_WORD> __sanitize_type;
	__sanitize_type::_S_do_sanitize(this->_M_hiword());
      }

    public:
      class reference
      {
	friend class bitset;

	_WordT*	_M_wp;
	size_type 	_M_bpos;

      public:
	reference&
	flip()
	{
	  *_M_wp ^= _Base::_S_maskbit(_M_bpos);
	  return *this;
	}
      };

      bitset<_Nb>&
      operator&=(const bitset<_Nb>& __rhs)
      {
	this->_M_do_and(__rhs);
	return *this;
      }

      bitset<_Nb>&
      flip()
      {
	this->_M_do_flip();
	this->_M_do_sanitize();
	return *this;
      }

      bitset<_Nb>
      operator~() const
      { return bitset<_Nb>(*this).flip(); }

      bool
      operator==(const bitset<_Nb>& __rhs) const
      { return this->_M_is_equal(__rhs); }

      bool
      any() const
      { return this->_M_is_any(); }
    };

  template<size_type _Nb>
    inline bitset<_Nb>
    operator&(const bitset<_Nb>& __x, const bitset<_Nb>& __y)
    {
      bitset<_Nb> __result(__x);
      __result &= __y;
      return __result;
    }
}
template<typename T>
class ArrayRef {
public:
    typedef const T *iterator;

private:
    const T *Data;
    size_type Length;

public:
    iterator begin() const { return Data; }
    iterator end() const { return Data + Length; }
};

const unsigned MAX_SUBTARGET_FEATURES = 128;
class FeatureBitset : public std::bitset<MAX_SUBTARGET_FEATURES> {
};

struct SubtargetFeatureKV {
  FeatureBitset Value;
  FeatureBitset Implies;
};

struct SubtargetInfoKV {
  const void *Value;
};
class SubtargetFeatures {
public:
    FeatureBitset ToggleFeature(FeatureBitset Bits,
				const SubtargetFeatureKV *,
				ArrayRef<SubtargetFeatureKV> FeatureTable);
};

static
void ClearImpliedBits(FeatureBitset &Bits,
		      const SubtargetFeatureKV *FeatureEntry,
		      ArrayRef<SubtargetFeatureKV> FeatureTable) {
  for (auto &FE : FeatureTable) {
    if ((FE.Implies & FeatureEntry->Value).any()) {
      Bits &= ~FE.Value;
      ClearImpliedBits(Bits, &FE, FeatureTable);
    }
  }
}

FeatureBitset
SubtargetFeatures::ToggleFeature(FeatureBitset Bits,
				 const SubtargetFeatureKV *FeatureEntry,
				 ArrayRef<SubtargetFeatureKV> FeatureTable) {
    if ((Bits & FeatureEntry->Value) == FeatureEntry->Value) {
      Bits &= ~FeatureEntry->Value;
      ClearImpliedBits(Bits, FeatureEntry, FeatureTable);
    }
  return Bits;
}

// { dg-final { scan-rtl-dump-not "S16 A32\[^\n\]*\\\*xorv4si3" "final" } }
