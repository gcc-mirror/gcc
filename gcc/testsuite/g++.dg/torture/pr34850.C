/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef uint8_t byte;
typedef uint32_t u32bit;
typedef unsigned int size_t;
extern "C" {
    extern void __warn_memset_zero_len (void) __attribute__((__warning__ ("")));
    extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__gnu_inline__, __artificial__))
    void * memset (void *__dest, int __ch, size_t __len) throw () {
	if (__builtin_constant_p (__len) && __len == 0)
	    __warn_memset_zero_len (); /* { dg-warning "" } */
    }
}
inline void clear_mem(void* ptr, u32bit n)    {
    memset(ptr, 0, n);
}
template<typename T> class MemoryRegion    {
public:
    u32bit size() const {
    }
    const T* begin() const {
    }
    void set(const T in[], u32bit n) {
	create(n);
    }
    void set(const MemoryRegion<T>& in) {
	set(in.begin(), in.size());
    }
    void clear() {
	clear_mem(buf, allocated);
    }
    void create(u32bit);
    MemoryRegion() {
	used = allocated = 0;
    }
    mutable T* buf;
    mutable u32bit used;
    mutable u32bit allocated;
};
template<typename T> void MemoryRegion<T>::create(u32bit n)    {
    if(n <= allocated) {
	clear();
    }
}
template<typename T> class SecureVector : public MemoryRegion<T>    {
public:
    SecureVector<T>& operator=(const MemoryRegion<T>& in)          {
	if(this != &in) set(in);
    }
};
class OctetString    {
public:
    SecureVector<byte> bits_of() const {
    }
    OctetString& operator^=(const OctetString&);
    void change(const MemoryRegion<byte>& in) {
	bits = in;
    }
    OctetString(const MemoryRegion<byte>& in) {
	change(in);
    }
    SecureVector<byte> bits;
};
OctetString& OctetString::operator^=(const OctetString& k)    {
    if(&k == this) {
	bits.clear();
    }
}
bool operator==(const OctetString& s1, const OctetString& s2)    {
    return (s1.bits_of() == s2.bits_of());
}
