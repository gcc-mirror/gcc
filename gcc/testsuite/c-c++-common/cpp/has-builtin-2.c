/* PR c/66970 - Add __has_builtin() macro
   Verify __has_builtin evaluation for common built-ins and other identifiers.
   { dg-do compile } */

// Verify a few library built-ins.
#if !__has_builtin (__builtin_abs)
#  error "__has_builtin (__builtin_abs) failed"
#endif

#if !__has_builtin (abs)
   // abs is also a built-in unless disabled by -fno-builtin.
#  error "__has_builtin (abs) failed"
#endif

#if __cplusplus
// Declare an overload and verify that __has_builtin (isalpha) still
// evaluates to true.
int isalpha (const char*);
#endif

#if !__has_builtin (__builtin_isalpha)
#  error "__has_builtin (__builtin_isalpha) failed"
#endif

#if !__has_builtin (isalpha)
   // isalpha is still a built-in despite the overload above.
#  error "__has_builtin (isalpha) failed"
#endif


#if !__has_builtin (__builtin__Exit)
#  error "__has_builtin (__builtin__Exit) failed"
#endif


#if !__has_builtin (__builtin_alloca)
#  error "__has_builtin (__builtin_alloca) failed"
#endif

#if !__has_builtin (__builtin_is_constant_evaluated)
   // __builtin_is_constant_evaluated is a C++-only built.
#  ifdef __cplusplus
#    error "__has_builtin (__builtin_is_constant_evaluated) failed"
#  endif
#else
#  ifndef __cplusplus
#    error "__has_builtin (__builtin_is_constant_evaluated) failed"
#  endif
#endif

#if !__has_builtin (__builtin_expect)
#  error "__has_builtin (__builtin_expect) failed"
#endif

#if !__has_builtin (__builtin_trap)
#  error "__has_builtin (__builtin_trap) failed"
#endif

#if !__has_builtin (__builtin_unreachable)
#  error "__has_builtin (__builtin_unreachable) failed"
#endif

#if !__has_builtin (__builtin_LINE)
#  error "__has_builtin (__builtin_LINE) failed"
#endif

#if !__has_builtin (__builtin_object_size)
#  error "__has_builtin (__builtin_object_size) failed"
#endif

#if !__has_builtin (__builtin_inf)
#  error "__has_builtin (__builtin_inf) failed"
#endif

#if !__has_builtin (__builtin_nan)
#  error "__has_builtin (__builtin_nan) failed"
#endif

#if !__has_builtin (__builtin_bswap16)
#  error "__has_builtin (__builtin_bswap16) failed"
#endif

#if !__has_builtin (__builtin_bswap32)
#  error "__has_builtin (__builtin_bswap32) failed"
#endif


// Verify a few integer overflow built-ins.
#if !__has_builtin (__builtin_add_overflow)
#  error "__has_builtin (__builtin_add_overflow) failed"
#endif

#if !__has_builtin (__builtin_sadd_overflow)
#  error "__has_builtin (__builtin_sadd_overflow) failed"
#endif

#if !__has_builtin (__builtin_add_overflow_p)
#  error "__has_builtin (__builtin_add_overflow_p) failed"
#endif


// Verify a few atomic built-ins.
#if !__has_builtin (__atomic_load)
#  error "__has_builtin (__atomic_load) failed"
#endif

#if !__has_builtin (__atomic_load_n)
#  error "__has_builtin (__atomic_load_n) failed"
#endif

#if !__has_builtin (__atomic_store)
#  error "__has_builtin (__atomic_store) failed"
#endif

#if !__has_builtin (__atomic_store_n)
#  error "__has_builtin (__atomic_store_n) failed"
#endif

#if !__has_builtin (__atomic_exchange)
#  error "__has_builtin (__atomic_echange) failed"
#endif

#if !__has_builtin (__atomic_exchange_n)
#  error "__has_builtin (__atomic_exchange_n) failed"
#endif


// Verify a few sync built-ins.
#if !__has_builtin (__sync_fetch_and_add)
#  error "__has_builtin (__sync_fetch_and_add) failed"
#endif

#if !__has_builtin (__sync_add_and_fetch)
#  error "__has_builtin (__sync_add_and_fetch) failed"
#endif

#if !__has_builtin (__sync_bool_compare_and_swap)
#  error "__has_builtin (__sync_bool_compare_and_swap) failed"
#endif

#if !__has_builtin (__sync_val_compare_and_swap)
#  error "__has_builtin (__sync_val_compare_and_swap) failed"
#endif

#if !__has_builtin (__sync_synchronize)
#  error "__has_builtin (__sync_synchronize) failed"
#endif

// Verify nonlocal goto builtins.
#if !__has_builtin (__builtin_setjmp)
#  error "__has_builtin (__builtin_setjmp) failed"
#endif

#if !__has_builtin (__builtin_longjmp)
#  error "__has_builtin (__builtin_longjmp) failed"
#endif


// Verify a few built-ins for constructing function calls.

#if !__has_builtin (__builtin_apply)
#  error "__has_builtin (__builtin_apply) failed"
#endif

#if !__has_builtin (__builtin_return)
#  error "__has_builtin (__builtin_return) failed"
#endif

// Verify built-ins for function return address.
#if !__has_builtin (__builtin_return_address)
#  error "__has_builtin (__builtin_return_address) failed"
#endif

#if !__has_builtin (__builtin_extract_return_addr)
#  error "__has_builtin (__builtin_extract_return_addr) failed"
#endif

#if !__has_builtin (__builtin_frob_return_addr)
#  error "__has_builtin (__builtin_frob_return_addr) failed"
#endif

#if !__has_builtin (__builtin_frame_address)
#  error "__has_builtin (__builtin_frame_address) failed"
#endif

// Verify keywords that aren't declared built-in functions.

#if !__has_builtin (__builtin_has_attribute)
#  error "__has_builtin (__builtin_has_attribute) failed"
#endif

#if !__has_builtin (__builtin_offsetof)
#  error "__has_builtin (__builtin_offsetof) failed"
#endif

// Verify some C-only built-ins.

#if !__has_builtin (__builtin_types_compatible_p)
#  if !__cplusplus
#    error "__has_builtin (__builtin_types_compatible_p) failed"
#  endif
#else
#  if __cplusplus
#    error "__has_builtin (__builtin_types_compatible_p) failed"
#  endif
#endif

// Verify a few C++ traits built-ins.

#if !__has_builtin (__builtin_addressof)
#  if __cplusplus
#    error "__has_builtin (__builtin_addressof) failed"
#  endif
#else
#  if !__cplusplus
#    error "__has_builtin (__builtin_addressof) failed"
#  endif
#endif

#if !__has_builtin (__builtin_launder)
#  if __cplusplus
#    error "__has_builtin (__builtin_launder) failed"
#  endif
#else
#  if !__cplusplus
#    error "__has_builtin (__builtin_launder) failed"
#  endif
#endif

#if !__has_builtin (__has_nothrow_assign)
#  if __cplusplus
#    error "__has_builtin (__has_nothrow_assign) failed"
#  endif
#else
#  if !__cplusplus
#    error "__has_builtin (__has_nothrow_assign) failed"
#  endif
#endif

#if !__has_builtin (__has_trivial_assign)
#  if __cplusplus
#    error "__has_builtin (__has_trivial_assign) failed"
#  endif
#else
#  if !__cplusplus
#    error "__has_builtin (__has_trivial_assign) failed"
#  endif
#endif

#if !__has_builtin (__has_virtual_destructor)
#  if __cplusplus
#    error "__has_builtin (__has_virtual_destructor) failed"
#  endif
#else
#  if !__cplusplus
#    error "__has_builtin (__has_virtual_destructor) failed"
#  endif
#endif


// Verify an Intel built-in that's not implemented by any other target.
#if !__has_builtin (__builtin_ia32_pause)
#  if defined (__i386__) || defined (__x86_64__)
#    error "__has_builtin (__builtin_ia32_pause) failed"
#  endif
#else
#  if !defined (__i386__) && !defined (__x86_64__)
#    error "__has_builtin (__builtin_ia32_pause) failed"
#  endif
#endif


// Verify non-functions.

#if __has_builtin (__alignof__)
#  error "__has_builtin (__alignof__) failed"
#endif

#if __has_builtin (asm)
#  error "__has_builtin (asm) failed"
#endif

#if __has_builtin (__asm__)
#  error "__has_builtin (__asm__) failed"
#endif

#if __has_builtin (__attribute__)
#  error "__has_builtin (__attribute__) failed"
#endif

#if __has_builtin (__inline__)
#  error "__has_builtin (__inline__) failed"
#endif

#if __has_builtin (__typeof__)
#  error "__has_builtin (__typeof__) failed"
#endif
