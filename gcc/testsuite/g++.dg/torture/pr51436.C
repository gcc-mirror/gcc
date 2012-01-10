/* { dg-do compile } */

typedef __SIZE_TYPE__ size_t;
extern "C" void *memcpy (void *, __const void *, size_t);
template < class Dest, class Source > struct BitCastHelper {
    static Dest cast (const Source & source) __attribute__ ((always_inline)) {
	Dest dest;
	memcpy (0, &source, sizeof dest);
    }
};
template < class Dest, class Source > Dest BitCast (Source)
{
  BitCastHelper < Dest, Source >::cast (0);
}

class MaybeObject
{
};
class Object:MaybeObject
{
public:
    static Object *cast (Object *) {
    }
};
class HeapObject:public Object
{
};
class String:public HeapObject
{
};
class ExternalString:public String
{
};
class ExternalTwoByteString:public ExternalString
{
};

template < typename T > class Handle
{
public:
    Handle () {
    }
    T *operator* () const;
    template < class S > static Handle < T > cast (Handle < S > that) {
	T::cast (*that);
    }
    T **location_;
};

template < typename T > T * Handle < T >::operator* () const
{
  *BitCast < T ** >(location_);
}

void
TestCharacterStream ()
{
  Handle < String > uc16_string;
  Handle < ExternalTwoByteString >::cast (uc16_string);
}
