/* Check that one can use integral template argument for memory model argument
 * in atomic SFINAE.  */
// { dg-do compile { target c++17 } }

#include <type_traits>

template <typename T, int I, typename = void> 
struct is_available : std::false_type {}; 

template <typename T, int I> 
struct is_available<T, I,
  std::void_t<decltype(__atomic_load (std::declval<T *>(),
				      std::declval<T *>(), I)) >> 
  : std::true_type {}; 

static_assert(is_available<int,1>::value == true);
static_assert(is_available<int,10>::value == false);
