// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::define_static_object.

#include <meta>

constexpr int arr[]{1, 2, 3};
// LWG4483 use extract(reflect_constant_array())
// constexpr const int(*ptr)[3] = std::define_static_object(arr);
// static_assert( *ptr == std::define_static_array(arr).data() );
// static_assert( ptr = &std::meta::constant_of(arr) );

constexpr int marr[3][3]{1, 2, 3};
// LWG4483 array are not structural so this fail
// constexpr const int(*mptr)[3][3] = std::define_static_object(marr);
// static_assert( *mptr == std::define_static_array(marr).data() );
// static_assert( mptr = &std::meta::constant_of(marr) );

