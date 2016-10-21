// PR c++/77656
// { dg-do compile { target stdint_types } }

#include <stdint.h>

template<uint64_t _Val,
	 int _Val2 = (_Val >> 32)>
class Test {};

template<uint32_t _X>
class Test2 : Test<_X> {};

template<uint32_t _X>
class Test3 : Test<(uint64_t) _X> {};
