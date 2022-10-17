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
  if (x->elem_len != 3*sizeof(char))
    __builtin_abort ();
  if (x->version != CFI_VERSION)
    __builtin_abort ();
  if (x->rank != 1)
    __builtin_abort ();
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  if (x->type != CFI_type_char)
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
  for (size_t i = 0; i < len; ++i)
    __builtin_printf ("%c", ((const char*) p)[i]);
  __builtin_printf ("<\n");
}

static void
check_str (CFI_cdesc_t *x, const char *str, const CFI_index_t subscripts[])
{
  /* Avoid checking for '\0'.  */
  if (strncmp ((const char*) CFI_address (x, subscripts), str, strlen(str)) != 0)
    __builtin_abort ();
}

static void
set_str (CFI_cdesc_t *x, const char *str, const CFI_index_t subscripts[])
{
  char *p = CFI_address (x, subscripts);
  size_t len = strlen (str);
  if (x->elem_len != len)
    __builtin_abort ();
  for (size_t i = 0; i < len; ++i)
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
      check_str (x, "abc", zero);
      check_str (x, "ghi", one);
      check_str (x, "nop", two);
      check_str (y, "abc", zero);
      check_str (y, "ghi", one);
      check_str (y, "nop", two);
      check_str (z, "abc", zero);
      check_str (z, "ghi", one);
      check_str (z, "nop", two);
    }
  else if (num == 1)
    {
      if (strncmp ((const char*) x->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) y->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) z->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
    }
  else if (num == 2)
    {
      if (strncmp ((const char*) x->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) y->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) z->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
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
      check_str (x, "abc", zero);
      check_str (x, "ghi", one);
      check_str (x, "nop", two);
      check_str (y, "abc", zero);
      check_str (y, "ghi", one);
      check_str (y, "nop", two);
      check_str (z, "abc", zero);
      check_str (z, "ghi", one);
      check_str (z, "nop", two);
    }
  else if (intent_in && num == 1)
    {
      if (strncmp ((const char*) x->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) y->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) z->base_addr, "abcghinop", 9) != 0)
	__builtin_abort ();
    }
  else if (intent_in && num == 2)
    {
      if (strncmp ((const char*) x->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) y->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) z->base_addr, "defghijlm", 9) != 0)
	__builtin_abort ();
    }
  else if (intent_in)
    __builtin_abort ();
  if (intent_in)
    {
      if (is_cont && num == 1)
        {
	  /* Copy in - set the value to check that no copy out is done. */
	  memcpy ((char*) x->base_addr, "123456789", 9);
	  memcpy ((char*) y->base_addr, "123456789", 9);
	  memcpy ((char*) z->base_addr, "123456789", 9);
        }
      return addr1;
    }
  // !intent_in
  if (!is_cont && num == 1)
    {
      check_str (x, "ABC", zero);
      check_str (x, "DEF", one);
      check_str (x, "GHI", two);
      check_str (y, "ABC", zero);
      check_str (y, "DEF", one);
      check_str (y, "GHI", two);
      check_str (z, "ABC", zero);
      check_str (z, "DEF", one);
      check_str (z, "GHI", two);
    }
  else
    {
      if (strncmp ((const char*) x->base_addr, "ABCDEFGHI", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) y->base_addr, "ABCDEFGHI", 9) != 0)
	__builtin_abort ();
      if (strncmp ((const char*) z->base_addr, "ABCDEFGHI", 9) != 0)
	__builtin_abort ();
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
      set_str (x, "abc", zero);
      set_str (x, "ghi", one);
      set_str (x, "nop", two);
    }
  else if (num == 2)
    {
      set_str (x, "def", zero);
      set_str (x, "ghi", one);
      set_str (x, "jlm", two);
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
