
int ary[4];
extern int unb[];
typedef int z[0];


template<typename _Tp>
struct __aligned_membuf
{
  unsigned char _M_storage[sizeof(_Tp)];
  _Tp bob[5];

  typedef _Tp ary[5];
  typedef const ary c_ary;
};
