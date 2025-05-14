/* PR middle-end/93437 - bogus -Warray-bounds on protobuf generated code
   { dg-do compile }
   { dg-options "-O3 -Wall" } */

typedef __SIZE_TYPE__ size_t;
typedef __UINT32_TYPE__ uint32_t;

inline void* operator new (size_t, void* p)
{
  return p;
}

extern "C" void* memset(void*, int, size_t);

struct Bucket {
  Bucket ();
  uint32_t _has_bits_;
  uint32_t cumulative_count_;
  uint32_t upper_bound_;
};

extern Bucket _Bucket_default_instance_;

Bucket::Bucket ()
{
  memset (&_has_bits_, 0, sizeof _has_bits_);
  memset (&cumulative_count_, 0,    // { dg-bogus "\\\[-Warray-bounds|-Wstringop-overflow" }
	 static_cast<size_t>(reinterpret_cast<char*>(&upper_bound_)
			     - reinterpret_cast<char*>(&cumulative_count_))
	 + sizeof upper_bound_);
}

void* InitDefaultsBucket ()
{
  void* ptr = &::_Bucket_default_instance_;
  return new (ptr)::Bucket();
}
