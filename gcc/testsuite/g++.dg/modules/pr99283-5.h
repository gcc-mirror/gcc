template<typename _Value>
struct __traits
{
  static const int __digits = 8;
  static const _Value __min = 0;
};

template<typename _Value>
const _Value __traits<_Value>::__min;
