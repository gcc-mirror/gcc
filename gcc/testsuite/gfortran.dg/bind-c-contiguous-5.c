#include <ISO_Fortran_binding.h>
#include <stdbool.h>
#include <string.h>

struct loc_t {
  intptr_t x, y, z;
};

typedef struct loc_t (*ftn_fn) (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_size_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_size_in_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_expl_size_f (CFI_cdesc_t *,CFI_cdesc_t *, CFI_cdesc_t *,  int, int);
struct loc_t char_expl_size_in_f (CFI_cdesc_t *,CFI_cdesc_t *, CFI_cdesc_t *,  int, int);
struct loc_t char_assumed_rank_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_rank_in_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_rank_cont_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_rank_cont_in_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_shape_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_shape_in_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_shape_cont_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);
struct loc_t char_assumed_shape_cont_in_f (CFI_cdesc_t *, CFI_cdesc_t *, CFI_cdesc_t *, int, int);

static void
basic_check(CFI_cdesc_t *x, bool is_cont)
{
  if (!x->base_addr)
    __builtin_abort ();
  if (x->elem_len != 3*(4*sizeof(char))) /* ucs4_char */
    __builtin_abort ();
  if (x->version != CFI_VERSION)
    __builtin_abort ();
  if (x->rank != 1)
    __builtin_abort ();
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  if (x->type != CFI_type_ucs4_char)
    __builtin_abort ();
  if (x->dim[0].lower_bound != 0)
    __builtin_abort ();
  if (x->dim[0].extent != 3)
    __builtin_abort ();
  if (CFI_is_contiguous (x) != (x->elem_len == x->dim[0].sm))
    __builtin_abort ();
  if (is_cont != CFI_is_contiguous (x))
    __builtin_abort ();
}

static void
print_str (void *p, size_t len)
{
  __builtin_printf ("DEBUG: >");
  /* Use ' ' for '\0' */
  for (size_t i = 0; i < len*4; ++i)
    __builtin_printf ("%c", ((const char*) p)[i] ? ((const char*) p)[i] : ' ');
  __builtin_printf ("<\n");
}

static void
check_str (CFI_cdesc_t *x, const char *str, size_t n, const CFI_index_t subscripts[])
{
  /* Avoid checking for '\0'.  */
  if (memcmp ((const char*) CFI_address (x, subscripts), str, n) != 0)
    __builtin_abort ();
}

static void
set_str (CFI_cdesc_t *x, const char *str, size_t n, const CFI_index_t subscripts[])
{
  char *p = CFI_address (x, subscripts);
  if (x->elem_len != n)
    __builtin_abort ();
  for (size_t i = 0; i < n; ++i)
    p[i] = str[i];
}

static struct loc_t
do_call (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
	 int k, int num, bool intent_in, ftn_fn fn, bool is_cont, bool fort_cont)
{
  const CFI_index_t zero[1] = { 0 };
  const CFI_index_t one[1] = { 1 };
  const CFI_index_t two[1] = { 2 };
  struct loc_t addr1, addr2;
  if (k != 3)
    __builtin_abort ();
  basic_check (x, is_cont || num == 2);
  basic_check (y, is_cont || num == 2);
  basic_check (z, is_cont || num == 2);
  if (!is_cont && num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      check_str (x, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (x, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (x, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
      check_str (y, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (y, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (y, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
      check_str (z, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (z, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (z, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
      check_str (x, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (x, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (x, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
      check_str (y, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (y, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (y, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
      check_str (z, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (z, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (z, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
#else
#error "Unsupported __BYTE_ORDER__"
#endif
    }
  else if (num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      if (memcmp ((const char*) x->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
#else
      if (memcmp ((const char*) x->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
#endif
    }
  else if (num == 2)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      if (memcmp ((const char*) x->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9*4) != 0)
	__builtin_abort ();
#else
      if (memcmp ((const char*) x->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9*4) != 0)
	__builtin_abort ();
#endif
    }
  else
    __builtin_abort ();
  addr1.x = (intptr_t) x->base_addr;
  addr1.y = (intptr_t) y->base_addr;
  addr1.z = (intptr_t) z->base_addr;
  addr2 = fn (x, y, z, 3, num);
  if (!CFI_is_contiguous (x) && fort_cont)
    {
      /* Check for callee copy in/copy out.  */
      if (addr1.x == addr2.x || addr1.x != (intptr_t) x->base_addr)
	__builtin_abort ();
      if (addr1.y == addr2.y || addr1.y != (intptr_t) y->base_addr)
	__builtin_abort ();
      if (addr1.z == addr2.z || addr1.z != (intptr_t) z->base_addr)
	__builtin_abort ();
    }
  else
    {
      if (addr1.x != addr2.x || addr1.x != (intptr_t) x->base_addr)
	__builtin_abort ();
      if (addr1.y != addr2.y || addr1.y != (intptr_t) y->base_addr)
	__builtin_abort ();
      if (addr1.z != addr2.z || addr1.z != (intptr_t) z->base_addr)
	__builtin_abort ();
    }
  // intent_in
  if (intent_in && !is_cont && num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      check_str (x, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (x, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (x, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
      check_str (y, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (y, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (y, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
      check_str (z, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      check_str (z, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      check_str (z, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
#else
      check_str (x, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (x, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (x, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
      check_str (y, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (y, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (y, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
      check_str (z, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      check_str (z, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      check_str (z, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
#endif
    }
  else if (intent_in && num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      if (memcmp ((const char*) x->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p\0\0\0", 9*4) != 0)
	__builtin_abort ();
#else
      if (memcmp ((const char*) x->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "\0\0\0a\0\0\0b\0\0\0c\0\0\0g\0\0\0h\0\0\0i\0\0\0n\0\0\0o\0\0\0p", 9*4) != 0)
	__builtin_abort ();
#endif
    }
  else if (intent_in && num == 2)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      if (memcmp ((const char*) x->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m\0\0\0", 9) != 0)
	__builtin_abort ();
#else
      if (memcmp ((const char*) x->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "\0\0\0d\0\0\0e\0\0\0f\0\0\0g\0\0\0h\0\0\0i\0\0\0j\0\0\0l\0\0\0m", 9) != 0)
	__builtin_abort ();
#endif
    }
  else if (intent_in)
    __builtin_abort ();
  if (intent_in)
    {
      if (is_cont && num == 1)
        {
	  /* Copy in - set the value to check that no copy out is done. */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
	  memcpy ((char*) x->base_addr, "1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9\0\0\0", 9*4);
	  memcpy ((char*) y->base_addr, "1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9\0\0\0", 9*4);
	  memcpy ((char*) z->base_addr, "1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9\0\0\0", 9*4);
#else
	  memcpy ((char*) x->base_addr, "\0\0\0""1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9", 9*4);
	  memcpy ((char*) y->base_addr, "\0\0\0""1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9", 9*4);
	  memcpy ((char*) z->base_addr, "\0\0\0""1\0\0\0""2\0\0\0""3\0\0\0""4\0\0\0""5\0\0\0""6\0\0\0""7\0\0\0""8\0\0\0""9", 9*4);
#endif
        }
      return addr1;
    }
  // !intent_in
  if (!is_cont && num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      check_str (x, "A\0\0\0B\0\0\0C\0\0\0", 3*4, zero);
      check_str (x, "D\0\0\0E\0\0\0F\0\0\0", 3*4, one);
      check_str (x, "G\0\0\0H\0\0\0I\0\0\0", 3*4, two);
      check_str (y, "A\0\0\0B\0\0\0C\0\0\0", 3*4, zero);
      check_str (y, "D\0\0\0E\0\0\0F\0\0\0", 3*4, one);
      check_str (y, "G\0\0\0H\0\0\0I\0\0\0", 3*4, two);
      check_str (z, "A\0\0\0B\0\0\0C\0\0\0", 3*4, zero);
      check_str (z, "D\0\0\0E\0\0\0F\0\0\0", 3*4, one);
      check_str (z, "G\0\0\0H\0\0\0I\0\0\0", 3*4, two);
#else
      check_str (x, "\0\0\0A\0\0\0B\0\0\0C", 3*4, zero);
      check_str (x, "\0\0\0D\0\0\0E\0\0\0F", 3*4, one);
      check_str (x, "\0\0\0G\0\0\0H\0\0\0I", 3*4, two);
      check_str (y, "\0\0\0A\0\0\0B\0\0\0C", 3*4, zero);
      check_str (y, "\0\0\0D\0\0\0E\0\0\0F", 3*4, one);
      check_str (y, "\0\0\0G\0\0\0H\0\0\0I", 3*4, two);
      check_str (z, "\0\0\0A\0\0\0B\0\0\0C", 3*4, zero);
      check_str (z, "\0\0\0D\0\0\0E\0\0\0F", 3*4, one);
      check_str (z, "\0\0\0G\0\0\0H\0\0\0I", 3*4, two);
#endif
    }
  else
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      if (memcmp ((const char*) x->base_addr, "A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I\0\0\0", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I\0\0\0", 9*4) != 0)
	__builtin_abort ();
#else
      if (memcmp ((const char*) x->base_addr, "\0\0\0A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) y->base_addr, "\0\0\0A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I", 9*4) != 0)
	__builtin_abort ();
      if (memcmp ((const char*) z->base_addr, "\0\0\0A\0\0\0B\0\0\0C\0\0\0D\0\0\0E\0\0\0F\0\0\0G\0\0\0H\0\0\0I", 9*4) != 0)
	__builtin_abort ();
#endif
    }
  return addr1;
}

struct loc_t
char_assumed_size_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  return do_call (x, y, z, k, num, false, char_assumed_size_f, true, false);
}

struct loc_t
char_assumed_size_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
			int k, int num)
{
  return do_call (x, y, z, k, num, true, char_assumed_size_in_f, true, false);
}

struct loc_t
char_expl_size_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		  int k, int num)
{
  return do_call (x, y, z, k, num, false, char_expl_size_f, true, false);
}

struct loc_t
char_expl_size_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  return do_call (x, y, z, k, num, true, char_expl_size_in_f, true, false);
}

struct loc_t
char_assumed_rank_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  return do_call (x, y, z, k, num, false, char_assumed_rank_f, false, false);
}

struct loc_t
char_assumed_rank_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  return do_call (x, y, z, k, num, true, char_assumed_rank_in_f, false, false);
}

struct loc_t
char_assumed_rank_cont_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
			  int k, int num)
{
  return do_call (x, y, z, k, num, false, char_assumed_rank_cont_f, true, false);
}

struct loc_t
char_assumed_rank_cont_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
			  int k, int num)
{
  return do_call (x, y, z, k, num, true, char_assumed_rank_cont_in_f, true, false);
}

static void
reset_var (CFI_cdesc_t *x, int num)
{
  const CFI_index_t zero[1] = { 0 };
  const CFI_index_t one[1] = { 1 };
  const CFI_index_t two[1] = { 2 };

  if (num == 1)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      set_str (x, "a\0\0\0b\0\0\0c\0\0\0", 3*4, zero);
      set_str (x, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      set_str (x, "n\0\0\0o\0\0\0p\0\0\0", 3*4, two);
#else
      set_str (x, "\0\0\0a\0\0\0b\0\0\0c", 3*4, zero);
      set_str (x, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      set_str (x, "\0\0\0n\0\0\0o\0\0\0p", 3*4, two);
#endif
    }
  else if (num == 2)
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      set_str (x, "d\0\0\0e\0\0\0f\0\0\0", 3*4, zero);
      set_str (x, "g\0\0\0h\0\0\0i\0\0\0", 3*4, one);
      set_str (x, "j\0\0\0l\0\0\0m\0\0\0", 3*4, two);
#else
      set_str (x, "\0\0\0d\0\0\0e\0\0\0f", 3*4, zero);
      set_str (x, "\0\0\0g\0\0\0h\0\0\0i", 3*4, one);
      set_str (x, "\0\0\0j\0\0\0l\0\0\0m", 3*4, two);
#endif
    }
  else
    __builtin_abort ();
}

static void
reset_vars (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z, int num)
{
  reset_var (x, num);
  reset_var (y, num);
  reset_var (z, num);
}

struct loc_t
char_assumed_shape_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  /* Make use of having a noncontiguous argument to check that the callee
     handles noncontiguous variables.  */
  do_call (x, y, z, k, num, false, char_assumed_size_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, true, char_assumed_size_in_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, false, char_expl_size_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, true, char_expl_size_in_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, false, char_assumed_rank_cont_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, true, char_assumed_rank_cont_in_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, false, char_assumed_shape_cont_f, false, true);
  reset_vars (x, y, z, num);
  do_call (x, y, z, k, num, true, char_assumed_shape_cont_in_f, false, true);
  /* Actual func call. */
  reset_vars (x, y, z, num);
  return do_call (x, y, z, k, num, false, char_assumed_shape_f, false, false);
}

struct loc_t
char_assumed_shape_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
		     int k, int num)
{
  return do_call (x, y, z, k, num, true, char_assumed_shape_in_f, false, false);
}

struct loc_t
char_assumed_shape_cont_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
			  int k, int num)
{
  return do_call (x, y, z, k, num, false, char_assumed_shape_cont_f, true, false);
}

struct loc_t
char_assumed_shape_cont_in_c (CFI_cdesc_t *x, CFI_cdesc_t *y, CFI_cdesc_t *z,
			  int k, int num)
{
  return do_call (x, y, z, k, num, true, char_assumed_shape_cont_in_f, true, false);
}
