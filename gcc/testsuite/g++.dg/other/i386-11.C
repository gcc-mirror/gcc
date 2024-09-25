// PR target/80799
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -msse2" }
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <xmmintrin.h>
#include <emmintrin.h>

class alignas(16) GSVector4i
{
public:
    __m128i m;

	explicit GSVector4i(__m128i m)
	{
		this->m = m;
	}

	static void storel(void* p, const GSVector4i& v)
	{
		_mm_storel_epi64((__m128i*)p, v.m);
	}

	static GSVector4i loadl(const void* p)
	{
		return GSVector4i(_mm_loadl_epi64((__m128i*)p));
	}

	bool eq(const GSVector4i& v) const
	{
		return _mm_movemask_epi8(_mm_cmpeq_epi32(m, v.m)) == 0xffff;
	}
};


union GIFRegTRXPOS
{
	unsigned long long u64;
	void operator = (const GSVector4i& v) {GSVector4i::storel(this, v);}
	bool operator != (const union GIFRegTRXPOS& r) const {return !((GSVector4i)r).eq(*this);}
	operator GSVector4i() const {return GSVector4i::loadl(this);}
};

extern void dummy_call();
extern GIFRegTRXPOS TRXPOS;

void GIFRegHandlerTRXPOS(const GIFRegTRXPOS&  p)
{
	if(p != TRXPOS)
	{
		dummy_call();
	}

	TRXPOS = (GSVector4i)p;
}

// { dg-final { scan-assembler-not "%mm" } }
