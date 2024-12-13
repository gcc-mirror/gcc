/* Check that overloaded builtins still error when not in SFINAE context.  */
// { dg-do compile { target c++17 } }
#include <type_traits>

/* Should error here due to the fully specified (and invalid) builtin calls.  */
#define SFINAE_TYPE_CHECK(NAME, PARAMS, NONPOINTER_PARAMS) \
  template <typename T, typename = void> \
  struct is_##NAME##_available : std::false_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int(), int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int(), int(), int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (X(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (Incomplete(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (int(), int(), int(), int(), \
					  int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (std::declval<int*>(), int(), int(), int(), \
					  int(), int(), int())) >> \
    : std::true_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME ()) >> \
    : std::true_type {};

/* All the errors that are emitted.  We don't check that the errors directly
   correspond to the relevant scenarios because validation that the correct
   errors are generated for the relevant problems is done in other tests.

   This test is checking that all the error types below are still emitted
   when the problem occurs in templates for fully specified calls.

   NOTE: We are missing some of the errors that could be emitted because the
   above doesn't generate all invalid calls.
   Things that could be added:
      - pointer to incomplete type
      - pointer to type of non-constant size
      - pointer to type of zero size
      - arguments after the first one:
	- not pointers
	- pointers to non-constant sized type
	- pointers to function
	- pointer to type of different size to the first one.
	- pointer to const type
	- pointer to volatile type
      - memory model argument is not an integral type
      - all errors around bitint
      */

/* { dg-error "argument 1 of '__atomic_compare_exchange' must be a non-void pointer type"                  "" { target *-*-* } 31 } */
/* { dg-error "argument 1 of '__atomic_exchange' must be a non-void pointer type"                          "" { target *-*-* } 23 } */
/* { dg-error "argument 1 of '__atomic_load' must be a non-void pointer type"                              "" { target *-*-* } 19 } */
/* { dg-error "argument 1 of '__atomic_store' must be a non-void pointer type"                             "" { target *-*-* } 19 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 11 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 15 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 19 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 23 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 27 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 35 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 39 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 43 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 48 } */
/* { dg-error "incorrect number of arguments to function '__atomic_compare_exchange'"                      "" { target *-*-* } 53 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 11 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 15 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 19 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 27 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 31 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 35 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 39 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 43 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 48 } */
/* { dg-error "incorrect number of arguments to function '__atomic_exchange'"                              "" { target *-*-* } 53 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 11 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 15 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 23 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 27 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 31 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 35 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 39 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 43 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 48 } */
/* { dg-error "incorrect number of arguments to function '__atomic_load'"                                  "" { target *-*-* } 53 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 11 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 15 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 23 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 27 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 31 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 35 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 39 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 43 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 48 } */
/* { dg-error "incorrect number of arguments to function '__atomic_store'"                                 "" { target *-*-* } 53 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_add_fetch'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_and_fetch'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_compare_exchange_n'" "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_exchange_n'"         "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_add'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_and'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_nand'"         "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_or'"           "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_sub'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_fetch_xor'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_load_n'"             "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_nand_fetch'"         "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_or_fetch'"           "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_store_n'"            "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_sub_fetch'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'Incomplete' is incompatible with argument 1 of '__atomic_xor_fetch'"          "" { target *-*-* } 39 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_add_fetch'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_and_fetch'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_compare_exchange_n'"        "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_exchange_n'"                "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_add'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_and'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_nand'"                "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_or'"                  "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_sub'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_fetch_xor'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_load_n'"                    "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_nand_fetch'"                "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_or_fetch'"                  "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_store_n'"                   "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_sub_fetch'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 11 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 15 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 19 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 23 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 27 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 31 } */
/* { dg-error "operand type 'int' is incompatible with argument 1 of '__atomic_xor_fetch'"                 "" { target *-*-* } 43 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_add_fetch'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_and_fetch'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_compare_exchange_n'"          "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_exchange_n'"                  "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_add'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_and'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_nand'"                  "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_or'"                    "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_sub'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_fetch_xor'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_load_n'"                      "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_nand_fetch'"                  "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_or_fetch'"                    "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_store_n'"                     "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_sub_fetch'"                   "" { target *-*-* } 35 } */
/* { dg-error "operand type 'X' is incompatible with argument 1 of '__atomic_xor_fetch'"                   "" { target *-*-* } 35 } */
/* { dg-error "too few arguments to function '__atomic_add_fetch'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_and_fetch'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_compare_exchange_n'"                                "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_exchange_n'"                                        "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_add'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_and'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_nand'"                                        "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_or'"                                          "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_sub'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_fetch_xor'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_load_n'"                                            "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_nand_fetch'"                                        "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_or_fetch'"                                          "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_store_n'"                                           "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_sub_fetch'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too few arguments to function '__atomic_xor_fetch'"                                         "" { target *-*-* } 53 } */
/* { dg-error "too many arguments to function '__atomic_add_fetch'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_and_fetch'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_compare_exchange_n'"                               "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_exchange_n'"                                       "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_add'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_and'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_nand'"                                       "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_or'"                                         "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_sub'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_fetch_xor'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_load_n'"                                           "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_nand_fetch'"                                       "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_or_fetch'"                                         "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_store_n'"                                          "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_sub_fetch'"                                        "" { target *-*-* } 48 } */
/* { dg-error "too many arguments to function '__atomic_xor_fetch'"                                        "" { target *-*-* } 48 } */


/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 11 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 15 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 19 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 23 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 27 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 31 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 35 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 39 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 43 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 48 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } 53 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 11 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 15 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 19 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 23 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 27 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 31 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 35 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 39 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 44 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 49 } */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } 53 } */

/* Just avoid generating anything for the assertions (not what we're testing
   here).  */
#define MAKE_ATOMIC_ASSERT(NAME, TYPE, SUCCESS)

#include "builtin-atomic-overloads.def"
