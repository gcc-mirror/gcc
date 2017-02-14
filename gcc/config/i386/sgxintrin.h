#ifndef _SGXINTRIN_H_INCLUDED
#define _SGXINTRIN_H_INCLUDED

#ifndef __SGX__
#pragma GCC push_options
#pragma GCC target("sgx")
#define __DISABLE_SGX__
#endif /* __SGX__ */

#define __encls_bc(leaf, b, c, retval)	    	 	\
  __asm__ __volatile__ ("encls\n\t"		     	\
	   : "=a" (retval)			     	\
	   : "a" (leaf), "b" (b), "c" (c)		\
	   : "cc")

#define __encls_bcd(leaf, b, c, d, retval)	     	\
  __asm__ __volatile__("encls\n\t"		     	\
	   : "=a" (retval)			     	\
	   : "a" (leaf), "b" (b), "c" (c), "d" (d)	\
	   : "cc")

#define __encls_c(leaf, c, retval)		     	\
  __asm__ __volatile__("encls\n\t"		     	\
	   : "=a" (retval)			     	\
	   : "a" (leaf), "c" (c)			\
	   : "cc")

#define __encls_edbgrd(leaf, b, c, retval)	     	\
  __asm__ __volatile__("encls\n\t"		     	\
	   : "=a" (retval), "=b" (b)		     	\
	   : "a" (leaf), "c" (c))

#define __encls_generic(leaf, b, c, d, retval)   	\
  __asm__ __volatile__("encls\n\t"		     	\
	   : "=a" (retval), "=b" (b), "=c" (c), "=d" (d)\
	   : "a" (leaf), "b" (b), "c" (c), "d" (d)	\
	   : "cc")

#define __enclu_bc(leaf, b, c, retval)			\
  __asm__ __volatile__("enclu\n\t"			\
	   : "=a" (retval)				\
	   : "a" (leaf), "b" (b), "c" (c)		\
	   : "cc")

#define __enclu_bcd(leaf, b, c, d, retval)		\
  __asm__ __volatile__("enclu\n\t"			\
	   : "=a" (retval)				\
	   : "a" (leaf), "b" (b), "c" (c), "d" (d)	\
	   : "cc")

#define __enclu_eenter(leaf, b, c, retval)		\
  __asm__  __volatile__("enclu\n\t"			\
	   : "=a" (retval), "=c" (c)			\
	   : "a" (leaf), "b" (b), "c" (c)		\
	   : "cc")

#define __enclu_eexit(leaf, b, c, retval)		\
  __asm__  __volatile__("enclu\n\t"			\
	   : "=a" (retval), "=c" (c)			\
	   : "a" (leaf), "b" (b)			\
	   : "cc")

#define __enclu_generic(leaf, b, c, d, retval)		\
  __asm__ __volatile__("enclu\n\t"			\
	   : "=a" (retval), "=b" (b), "=c" (c), "=d" (d)\
	   : "a" (leaf), "b" (b), "c" (c), "d" (d)	\
	   : "cc")


extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_encls_u32 (const int __L, size_t __D[])
{
  enum __encls_type
  {
    __SGX_ECREATE = 0x00,
    __SGX_EADD    = 0x01,
    __SGX_EINIT   = 0x02,
    __SGX_EREMOVE = 0x03,
    __SGX_EDBGRD  = 0x04,
    __SGX_EDBGWR  = 0x05,
    __SGX_EEXTEND = 0x06,
    __SGX_ELDB    = 0x07,
    __SGX_ELDU    = 0x08,
    __SGX_EBLOCK  = 0x09,
    __SGX_EPA     = 0x0A,
    __SGX_EWB     = 0x0B,
    __SGX_ETRACK  = 0x0C,
    __SGX_EAUG    = 0x0D,
    __SGX_EMODPR  = 0x0E,
    __SGX_EMODT   = 0x0F
  };
  enum __encls_type __T = (enum __encls_type)__L;
  int __R = 0;
  if (!__builtin_constant_p (__T))
    __encls_generic (__L, __D[0], __D[1], __D[2], __R);
  else switch (__T)
    {
    case __SGX_ECREATE:
    case __SGX_EADD:
    case __SGX_EDBGWR:
    case __SGX_EEXTEND:
    case __SGX_EPA:
    case __SGX_EMODPR:
    case __SGX_EMODT:
    case __SGX_EAUG:
      __encls_bc (__L, __D[0], __D[1], __R);
      break;
    case __SGX_EINIT:
    case __SGX_ELDB:
    case __SGX_ELDU:
    case __SGX_EWB:
      __encls_bcd (__L, __D[0], __D[1], __D[2], __R);
      break;
    case __SGX_EREMOVE:
    case __SGX_EBLOCK:
    case __SGX_ETRACK:
      __encls_c (__L, __D[1], __R);
      break;
    case __SGX_EDBGRD:
      __encls_edbgrd (__L, __D[0], __D[1], __R);
      break;
    default:
      return -1;
    }
  return __R;
}

extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_enclu_u32 (const int __L, size_t __D[])
{
  enum __enclu_type
  {
    __SGX_EREPORT     = 0x00,
    __SGX_EGETKEY     = 0x01,
    __SGX_EENTER      = 0x02,
    __SGX_ERESUME     = 0x03,
    __SGX_EEXIT       = 0x04,
    __SGX_EACCEPT     = 0x05,
    __SGX_EMODPE      = 0x06,
    __SGX_EACCEPTCOPY = 0x07
  };
  enum __enclu_type __T = (enum __enclu_type) __L;
  int __R = 0;
  if (!__builtin_constant_p (__T))
    __enclu_generic (__L, __D[0], __D[1], __D[2], __R);
  else switch (__T)
    {
    case __SGX_EREPORT:
    case __SGX_EACCEPTCOPY:
      __enclu_bcd (__L, __D[0], __D[1], __D[2], __R);
      break;
    case __SGX_EGETKEY:
    case __SGX_ERESUME:
    case __SGX_EACCEPT:
    case __SGX_EMODPE:
      __enclu_bc (__L, __D[0], __D[1], __R);
      break;
    case __SGX_EENTER:
      __enclu_eenter (__L, __D[0], __D[1], __R);
      break;
    case __SGX_EEXIT:
      __enclu_eexit (__L, __D[0], __D[1], __R);
      break;
    default:
      return -1;
    }
  return __R;
}

#ifdef __DISABLE_SGX__
#undef __DISABLE_SGX__
#pragma GCC pop_options
#endif /* __DISABLE_SGX__ */

#endif /* _SGXINTRIN_H_INCLUDED */
