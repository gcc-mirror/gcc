/* XL compiler Hardware Transactional Memory (HTM) execution intrinsics.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.
   Contributed by Peter Bergner <bergner@vnet.ibm.com>.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef __HTM__
# error "HTM instruction set not enabled"
#endif /* __HTM__ */

#ifndef _HTMXLINTRIN_H
#define _HTMXLINTRIN_H

#include <stdint.h>
#include <htmintrin.h>

#ifdef __cplusplus
extern "C" {
#endif

#define _TEXASR_PTR(TM_BUF) \
  ((texasr_t *)((TM_BUF)+0))
#define _TEXASRU_PTR(TM_BUF) \
  ((texasru_t *)((TM_BUF)+0))
#define _TEXASRL_PTR(TM_BUF) \
  ((texasrl_t *)((TM_BUF)+4))
#define _TFIAR_PTR(TM_BUF) \
  ((tfiar_t *)((TM_BUF)+8))

typedef char TM_buff_type[16];

/* Compatibility macro with s390.  This macro can be used to determine
   whether a transaction was successfully started from the __TM_begin()
   and __TM_simple_begin() intrinsic functions below.  */
#define _HTM_TBEGIN_STARTED     1

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_simple_begin (void)
{
  if (__builtin_expect (__builtin_tbegin (0), 1))
    return _HTM_TBEGIN_STARTED;
  return 0;
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_begin (void* const TM_buff)
{
  *_TEXASRL_PTR (TM_buff) = 0;
  if (__builtin_expect (__builtin_tbegin (0), 1))
    return _HTM_TBEGIN_STARTED;
#ifdef __powerpc64__
  *_TEXASR_PTR (TM_buff) = __builtin_get_texasr ();
#else
  *_TEXASRU_PTR (TM_buff) = __builtin_get_texasru ();
  *_TEXASRL_PTR (TM_buff) = __builtin_get_texasr ();
#endif
  *_TFIAR_PTR (TM_buff) = __builtin_get_tfiar ();
  return 0;
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_end (void)
{
  unsigned char status = _HTM_STATE (__builtin_tend (0));
  if (__builtin_expect (status, _HTM_TRANSACTIONAL))
    return 1;
  return 0;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_abort (void)
{
  __builtin_tabort (0);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_named_abort (unsigned char const code)
{
  __builtin_tabort (code);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_resume (void)
{
  __builtin_tresume ();
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_suspend (void)
{
  __builtin_tsuspend ();
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_user_abort (void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  return _TEXASRU_ABORT (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_named_user_abort (void* const TM_buff, unsigned char *code)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);

  *code = _TEXASRU_FAILURE_CODE (texasru);
  return _TEXASRU_ABORT (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_illegal (void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  return _TEXASRU_DISALLOWED (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_footprint_exceeded (void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  return _TEXASRU_FOOTPRINT_OVERFLOW (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_nesting_depth (void* const TM_buff)
{
  texasrl_t texasrl;

  if (_HTM_STATE (__builtin_ttest ()) == _HTM_NONTRANSACTIONAL)
    {
      texasrl = *_TEXASRL_PTR (TM_buff);
      if (!_TEXASR_FAILURE_SUMMARY (texasrl))
        texasrl = 0;
    }
  else
    texasrl = (texasrl_t) __builtin_get_texasr ();

  return _TEXASR_TRANSACTION_LEVEL (texasrl);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_nested_too_deep(void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  return _TEXASRU_NESTING_OVERFLOW (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_conflict(void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  /* Return TEXASR bits 11 (Self-Induced Conflict) through
     14 (Translation Invalidation Conflict).  */
  return (_TEXASRU_EXTRACT_BITS (texasru, 14, 4)) ? 1 : 0;
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_failure_persistent(void* const TM_buff)
{
  texasru_t texasru = *_TEXASRU_PTR (TM_buff);
  return _TEXASRU_FAILURE_PERSISTENT (texasru);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_failure_address(void* const TM_buff)
{
  return *_TFIAR_PTR (TM_buff);
}

extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__TM_failure_code(void* const TM_buff)
{
  return *_TEXASR_PTR (TM_buff);
}

#ifdef __cplusplus
}
#endif

#endif /* _HTMXLINTRIN_H */
