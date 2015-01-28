/* { dg-do compile } */
/* { dg-options "-O3 -g" } */
typedef int size_t;
memcpy (size_t) __attribute__ ((__nonnull__));
memccpy (void *) __attribute__ ((__nonnull__ (1)))
  __attribute__ ((__nonnull__ (1)));
strncpy (size_t) __attribute__ ((__nonnull__));
strcat (char) __attribute__ ((__nonnull__));
struct
{
int __locales} ((__nonnull__));
strerror (int) __attribute__ ((__leaf__));
strerror_r (int)
__asm__ ("__xpg_strerror_r");
strerror_l (int);
__bzero (void *) __attribute__ ((__nothrow__));
__attribute__ (())bzero (void *, size_t);
size_t
__strcspn_c1 (__const char *, int);
__strcspn_c1 (__const char *p1, int p2)
{
}

__strcspn_c2 (__const char *, int, int);
__attribute__ (())__strcspn_c2 (__const char *p1, int p2, int p3)
{
  size_t __result = p1[__result] != p1[__result];
}

__attribute__ (())__strcspn_c3 (__const * p1, int p2, int p3, int p4)
{
  register __result;
  while (p1[__result] != p4)
    ;
}

__strspn_c1 (char *p1, int p2)
{
  register __result = 0;
  while (p1[__result] == p2)
    ;
}

log ();
exp2 () __attribute__ (());
__attribute__ (())__signbitl ()
{
  union
  {
  long __i[3]} __u = { };
  __u.__i[2];
}

typedef int8_t;
typedef int16_t;
typedef int32_t;
typedef char uint8_t;
typedef unsigned uint16_t;
typedef uint32_t;
typedef unsigned uint64_t;
__assert_fail (char, __const, int, char) __attribute__ ((__noreturn__));
typedef struct _IO_FILE FILE;
(((__HI__))), stdin, crc32_context, g_10, g_14, g_50, g_58, g_57, g_89, g_138,
  g_224, g_223 __attribute__ ((__mode__ (__SI__)));
struct _IO_FILE
{
  int _IO_read_ptr;
  char _IO_write_ptr;
  char _IO_write_end;
int _IO_backup_base} __io_read_fn (), *stdout;
_IO_putc ();
remove (int, char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)));
__attribute__ (())vprintf (__builtin_va_list p1)
{
  _IO_getc ();
}

__attribute__ (())fgetc_unlocked (FILE * p1)
{
  (p1, 0) ? __uflow () : p1;
}

getc_unlocked (FILE * p1)
{
  (p1->_IO_read_ptr, 0) ? __uflow () : (unsigned) stdin;
}

fputc_unlocked (int p1, FILE * p2)
{
  (p2->_IO_write_end) ? ((unsigned) p1) : (p2->_IO_write_ptr = p1);
}

__attribute__ (())putc_unlocked (int p1, FILE * p2)
{
  __builtin_expect (p2->_IO_write_end, 0)
    ? ((unsigned) p1)
    : (p2->_IO_write_ptr) ? (p1) : (stdout->_IO_write_ptr = p1);
}

feof_unlocked (FILE p1)
{
}

platform_main_end (uint32_t p1, int p2)
{
}

safe_unary_minus_func_int8_t_s ()
{
}

safe_add_func_int8_t_s_s (int8_t p1, int8_t p2)
{
}

safe_sub_func_int8_t_s_s (int8_t p1, int8_t p2)
{
}

safe_mul_func_int8_t_s_s (int8_t p1, int8_t p2)
{
}

safe_mod_func_int8_t_s_s (int8_t p1, int8_t p2)
{
}

safe_div_func_int8_t_s_s ()
{
}

safe_lshift_func_int8_t_s_s (int8_t p1, int p2)
{
  p1 || p2;
}

static
safe_lshift_func_int8_t_s_u (int8_t p1, int p2)
{
}

safe_add_func_int16_t_s_s (int16_t p1, int16_t p2)
{
  return p1 + p2;
}

safe_sub_func_int16_t_s_s (uint8_t p1, uint8_t p2)
{
}

safe_sub_func_uint8_t_u_u ()
{
}

safe_mul_func_uint8_t_u_u ()
{
}

safe_div_func_uint8_t_u_u (uint8_t p1, uint8_t p2)
{
  return;
}

safe_lshift_func_uint8_t_u_s (uint8_t p1, int p2)
{
}

safe_lshift_func_uint8_t_u_u (uint8_t p1, int p2)
{
}

safe_rshift_func_uint8_t_u_s (uint8_t p1, int p2)
{
  p2 || p2 ? : p2;
}

static uint16_t
safe_add_func_uint16_t_u_u (uint16_t p1, uint16_t p2)
{
  return p1 + p2;
}

safe_sub_func_uint16_t_u_u (uint16_t p1)
{
}

safe_mul_func_uint32_t_u_u ()
{
}

safe_lshift_func_uint32_t_u_s (uint32_t p1, int p2)
{
  p2 || p2 ? : p1;
}

safe_lshift_func_uint32_t_u_u (uint32_t p1, unsigned p2)
{
  p2 ? : p1 << p2;
}

safe_add_func_uint64_t_u_u (uint64_t p1)
{
}

safe_mod_func_uint64_t_u_u (uint64_t p1, uint64_t p2)
{
}

safe_div_func_uint64_t_u_u (uint64_t p1, uint64_t p2)
{
}

static
safe_lshift_func_uint64_t_u_s (p1)
{
  p1 ? : (unsigned) p1;
}

safe_rshift_func_uint64_t_u_s (uint64_t p1)
{
  (int) p1 ? : p1;
}

safe_rshift_func_uint64_t_u_u (uint64_t p1, unsigned p2)
{
  p2 >> p2;
}

safe_add_func_float_f_f (float p1, float p2)
{
  (p2) ? : p1;
}

safe_sub_func_float_f_f (float p1)
{
}

safe_mul_func_float_f_f (float p1, float p2)
{
}

safe_div_func_float_f_f (p1)
{
  p1 >= 2147483647 ? : p1;
}

crc32_tab[], g_273[], g_497, g_703, g_895[], *g_910;
crc32_gentab ()
{
  uint32_t crc;
  for (;;)
    for (;;)
      if (crc)
	;
}

crc32_byte (p1)
{
  crc32_context = crc32_context ^ crc32_tab[crc32_context ^ p1 & 255];
}

crc32_8bytes (uint64_t p1)
{
  crc32_byte ();
  crc32_byte (p1 >> 1);
  crc32_byte ();
}

transparent_crc (uint64_t p1, char p2, int p3)
{
  crc32_8bytes (p1);
  if (p3)
    printf (crc32_context);
}

transparent_crc_bytes (char p1, int p2, char p3)
{
  for (;;)
    crc32_byte ();
}

static **g_222 = &g_223;
static func_6 (uint32_t, uint8_t, int32_t);
const func_33 (uint32_t, uint8_t);
uint16_t **func_42 ();
int32_t *func_46 (uint16_t * const *, int16_t);
static
func_1 ()
{
  uint8_t l_11;
  int32_t l_15[9];
  uint16_t l_796;
  uint32_t l_1161[1][3];
  uint16_t l_1487[2];
  int i;
  i = 0;
  for (i; i < 5; i++)
    l_1487[1] ^=
      func_6 (g_10, l_11,
	      (func_33 ((func_42, l_15[8], l_796), g_273), l_1161[0][2]));
  g_910 ? : __assert_fail ("", "", 44, __PRETTY_FUNCTION__);
}

int32_t
func_6 (uint32_t p1, uint8_t p2, int32_t p3)
{
  int32_t l_1162;
  int32_t *l_1175 = &g_224;
  uint16_t l_1225;
  g_89 = 0;
  for (; g_89 < 35; g_89 = safe_add_func_int16_t_s_s (g_89, 7))
    {
      *g_222 = func_46 (l_1225, 0)
	? : __assert_fail ("g_223", "t.c", 59, __PRETTY_FUNCTION__);
      if (*l_1175)
	for (g_703 = 0; g_703 > -17; --g_703)
	  for (p3 = 0; p3 < 15; p3 = safe_add_func_uint16_t_u_u (p3, 1))
	    for (g_138 = 0; g_138 <= 6; g_138 += 1)
	      for (l_1162 = 4; l_1162 >= 0; l_1162 -= 1)
		g_895[3] = g_10;
    }
}

const int8_t
func_33 (uint32_t p1, uint8_t p2)
{
  uint8_t l_1116;
  uint16_t l_1130;
  g_910 = func_46 (l_1130, p2), l_1116;
}

func_36 (uint16_t p1, uint16_t p2, uint16_t p3, uint32_t p4, uint16_t p5)
{
}

uint16_t **
func_42 ()
{
  uint16_t i;
  for (;;)
    for (; i < 10; i++)
      ;
}

*func_46 (uint16_t * const *p1, int16_t p2)
{
  return g_57;
}

main ()
{
  int print_hash_value = func_1 ();
  transparent_crc (g_10, "", print_hash_value);
  transparent_crc (g_14, "", print_hash_value);
}

