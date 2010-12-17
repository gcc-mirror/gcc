/* { dg-require-alias "" } */
/* { dg-require-visibility "" } */

#ifndef __USER_LABEL_PREFIX__
#define PREFIX ""
#else
#define xstr(s) str(s)
#define str(s)  #s
#define PREFIX  xstr(__USER_LABEL_PREFIX__)
#endif

typedef unsigned short int __uint16_t;
enum
{
  _ISupper = (1 << (0)), _ISlower = (1 << (1)), _ISalpha =
    (1 << (2)), _ISdigit = (1 << (3)), _ISxdigit = (1 << (4)), _ISspace =
    (1 << (5)), _ISprint = (1 << (6)), _ISgraph = (1 << (7)), _ISblank =
    (1 << (8)), _IScntrl = (1 << (9)), _ISpunct = (1 << (10)), _ISalnum =
    (1 << (11))
};
typedef __uint16_t __ctype_mask_t;
extern const __ctype_mask_t *__C_ctype_b;
extern
__typeof (__C_ctype_b)
   __C_ctype_b __asm__ (PREFIX "__GI___C_ctype_b")
  __attribute__ ((visibility ("hidden")));
     static const __ctype_mask_t __C_ctype_b_data[] = {
     };

const __ctype_mask_t *__C_ctype_b = __C_ctype_b_data + 128;
extern
__typeof (__C_ctype_b)
     __EI___C_ctype_b __attribute__ ((alias ("" "__GI___C_ctype_b")));
