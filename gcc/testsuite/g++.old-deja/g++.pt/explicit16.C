// Build don't link:
// GROUPS passed templates
template<int N_rank>
class Array;


template<class T>
class ArraySectionInfo {
public:
    enum { rank = 0 };
};


template<class T1>
class SliceInfo {
public:
  static const int rank = ArraySectionInfo<T1>::rank;

  typedef Array<rank> T_slice;
};

template<class T2>
typename  SliceInfo<T2>::T_slice
foo(T2 r2)
{
  return SliceInfo<T2>::T_slice();
}

