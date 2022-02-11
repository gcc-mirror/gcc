/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu11 -fgnu89-inline" } */
/* { dg-final { scan-assembler-not ".quad\[\\t \]+tunable_list" { target lp64 } } } */
/* { dg-final { scan-assembler-not ".long\[\\t \]+tunable_list" { target { ! lp64 } } } } */

typedef unsigned long int size_t;
typedef long long int intmax_t;
typedef unsigned long long int uintmax_t;
typedef unsigned long long int uint64_t;
typedef intmax_t tunable_num_t;
typedef union
{
  tunable_num_t numval;
  const char *strval;
} tunable_val_t;
enum
{
  HWCAP_X86_SSE2 = 1 << 0,
  HWCAP_X86_64 = 1 << 1,
  HWCAP_X86_AVX512_1 = 1 << 2
};
typedef void (*tunable_callback_t) (tunable_val_t *);
extern void *__minimal_malloc (size_t n)
    __attribute__ ((visibility ("hidden")));
extern int __libc_enable_secure __attribute__ ((section (".data.rel.ro")));
extern uint64_t _dl_strtoul (const char *, char **)
    __attribute__ ((visibility ("hidden")));
extern void _dl_fatal_printf (const char *fmt, ...)
    __attribute__ ((__format__ (__printf__, 1, 2), __noreturn__));
typedef enum
{
  glibc_rtld_nns,
  glibc_elision_skip_lock_after_retries,
  glibc_malloc_trim_threshold,
  glibc_malloc_perturb,
  glibc_cpu_x86_shared_cache_size,
  glibc_pthread_rseq,
  glibc_mem_tagging,
  glibc_elision_tries,
  glibc_elision_enable,
  glibc_malloc_hugetlb,
  glibc_cpu_x86_rep_movsb_threshold,
  glibc_malloc_mxfast,
  glibc_rtld_dynamic_sort,
  glibc_elision_skip_lock_busy,
  glibc_malloc_top_pad,
  glibc_cpu_x86_rep_stosb_threshold,
  glibc_cpu_x86_non_temporal_threshold,
  glibc_cpu_x86_shstk,
  glibc_pthread_stack_cache_size,
  glibc_cpu_hwcap_mask,
  glibc_malloc_mmap_max,
  glibc_elision_skip_trylock_internal_abort,
  glibc_malloc_tcache_unsorted_limit,
  glibc_cpu_x86_ibt,
  glibc_cpu_hwcaps,
  glibc_elision_skip_lock_internal_abort,
  glibc_malloc_arena_max,
  glibc_malloc_mmap_threshold,
  glibc_cpu_x86_data_cache_size,
  glibc_malloc_tcache_count,
  glibc_malloc_arena_test,
  glibc_pthread_mutex_spin_count,
  glibc_rtld_optional_static_tls,
  glibc_malloc_tcache_max,
  glibc_malloc_check,
} tunable_id_t;
typedef enum
{
  TUNABLE_TYPE_INT_32,
  TUNABLE_TYPE_UINT_64,
  TUNABLE_TYPE_SIZE_T,
  TUNABLE_TYPE_STRING
} tunable_type_code_t;
typedef struct
{
  tunable_type_code_t type_code;
  tunable_num_t min;
  tunable_num_t max;
} tunable_type_t;
typedef enum
{
  TUNABLE_SECLEVEL_SXID_ERASE = 0,
  TUNABLE_SECLEVEL_SXID_IGNORE = 1,
  TUNABLE_SECLEVEL_NONE = 2,
} tunable_seclevel_t;
struct _tunable
{
  const char name[42];
  tunable_type_t type;
  tunable_val_t val;
  _Bool initialized;
  tunable_seclevel_t security_level;
  const char env_alias[23];
};
typedef struct _tunable tunable_t;
extern _Bool unsigned_tunable_type (tunable_type_code_t t);

static tunable_t tunable_list[] __attribute__ ((section (".data.rel.ro"))) = {
  { "glibc"
    "."
    "rtld"
    "."
    "nns",
    { TUNABLE_TYPE_SIZE_T, 1, 16 },
    { .numval = 4 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "elision"
    "."
    "skip_lock_after_retries",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    { .numval = 3 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "trim_threshold",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_TRIM_THRESHOLD_" },
  { "glibc"
    "."
    "malloc"
    "."
    "perturb",
    { TUNABLE_TYPE_INT_32, 0, 0xff },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_PERTURB_" },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_shared_cache_size",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "pthread"
    "."
    "rseq",
    { TUNABLE_TYPE_INT_32, 0, 1 },
    { .numval = 1 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "mem"
    "."
    "tagging",
    { TUNABLE_TYPE_INT_32, 0, 255 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    { 0 } },
  { "glibc"
    "."
    "elision"
    "."
    "tries",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    { .numval = 3 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "elision"
    "."
    "enable",
    { TUNABLE_TYPE_INT_32, 0, 1 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "hugetlb",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_rep_movsb_threshold",
    { TUNABLE_TYPE_SIZE_T, 1, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "mxfast",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    { 0 } },
  { "glibc"
    "."
    "rtld"
    "."
    "dynamic_sort",
    { TUNABLE_TYPE_INT_32, 1, 2 },
    { .numval = 2 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "elision"
    "."
    "skip_lock_busy",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    { .numval = 3 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "top_pad",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_TOP_PAD_" },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_rep_stosb_threshold",
    { TUNABLE_TYPE_SIZE_T, 1, (18446744073709551615UL) },
    { .numval = 2048 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_non_temporal_threshold",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_shstk",
    { TUNABLE_TYPE_STRING, 0, 0 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "pthread"
    "."
    "stack_cache_size",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    { .numval = 41943040 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "hwcap_mask",
    { TUNABLE_TYPE_UINT_64, 0, (18446744073709551615UL) },
    { .numval = (HWCAP_X86_64 | HWCAP_X86_AVX512_1) },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    "LD_HWCAP_MASK" },
  { "glibc"
    "."
    "malloc"
    "."
    "mmap_max",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_MMAP_MAX_" },
  { "glibc"
    "."
    "elision"
    "."
    "skip_trylock_internal_abort",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    { .numval = 3 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "tcache_unsorted_limit",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_ibt",
    { TUNABLE_TYPE_STRING, 0, 0 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "cpu"
    "."
    "hwcaps",
    { TUNABLE_TYPE_STRING, 0, 0 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "elision"
    "."
    "skip_lock_internal_abort",
    { TUNABLE_TYPE_INT_32, 0, (2147483647) },
    { .numval = 3 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "arena_max",
    { TUNABLE_TYPE_SIZE_T, 1, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_ARENA_MAX" },
  { "glibc"
    "."
    "malloc"
    "."
    "mmap_threshold",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_MMAP_THRESHOLD_" },
  { "glibc"
    "."
    "cpu"
    "."
    "x86_data_cache_size",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "tcache_count",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "arena_test",
    { TUNABLE_TYPE_SIZE_T, 1, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_IGNORE,
    "MALLOC_ARENA_TEST" },
  { "glibc"
    "."
    "pthread"
    "."
    "mutex_spin_count",
    { TUNABLE_TYPE_INT_32, 0, 32767 },
    { .numval = 100 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "rtld"
    "."
    "optional_static_tls",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    { .numval = 512 },
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "tcache_max",
    { TUNABLE_TYPE_SIZE_T, 0, (18446744073709551615UL) },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    { 0 } },
  { "glibc"
    "."
    "malloc"
    "."
    "check",
    { TUNABLE_TYPE_INT_32, 0, 3 },
    {},
    ((void *)0),
    TUNABLE_SECLEVEL_SXID_ERASE,
    "MALLOC_CHECK_" },
};
extern void __tunables_init (char **);
extern void __tunables_print (void);
extern void __tunable_get_val (tunable_id_t, void *, tunable_callback_t);
extern void __tunable_set_val (tunable_id_t, tunable_val_t *, tunable_num_t *,
                               tunable_num_t *);
static __inline __attribute__ ((__always_inline__)) _Bool
tunable_val_lt (tunable_num_t lhs, tunable_num_t rhs, _Bool unsigned_cmp)
{
  if (unsigned_cmp)
    return (uintmax_t)lhs < (uintmax_t)rhs;
  else
    return lhs < rhs;
}
static __inline __attribute__ ((__always_inline__)) _Bool
tunable_val_gt (tunable_num_t lhs, tunable_num_t rhs, _Bool unsigned_cmp)
{
  if (unsigned_cmp)
    return (uintmax_t)lhs > (uintmax_t)rhs;
  else
    return lhs > rhs;
}
static __inline __attribute__ ((__always_inline__)) _Bool
tunable_is_name (const char *orig, const char *envname)
{
  for (; *orig != '\0' && *envname != '\0'; envname++, orig++)
    if (*orig != *envname)
      break;
  if (*orig == '\0' && *envname == '=')
    return 1;
  else
    return 0;
}
static char *
tunables_strdup (const char *in)
{
  size_t i = 0;
  while (in[i++] != '\0')
    ;
  char *out = __minimal_malloc (i + 1);
  if (out == ((void *)0))
    _dl_fatal_printf ("failed to allocate memory to process tunables\n");
  while (i-- > 0)
    out[i] = in[i];
  return out;
}
static char **
get_next_env (char **envp, char **name, size_t *namelen, char **val,
              char ***prev_envp)
{
  while (envp != ((void *)0) && *envp != ((void *)0))
    {
      char **prev = envp;
      char *envline = *envp++;
      int len = 0;
      while (envline[len] != '\0' && envline[len] != '=')
        len++;
      if (envline[len] == '\0')
        continue;
      *name = envline;
      *namelen = len;
      *val = &envline[len + 1];
      *prev_envp = prev;
      return envp;
    }
  return ((void *)0);
}
static void
do_tunable_update_val (tunable_t *cur, const tunable_val_t *valp,
                       const tunable_num_t *minp, const tunable_num_t *maxp)
{
  tunable_num_t val, min, max;
  if (cur->type.type_code == TUNABLE_TYPE_STRING)
    {
      cur->val.strval = valp->strval;
      cur->initialized = 1;
      return;
    }
  _Bool unsigned_cmp = unsigned_tunable_type (cur->type.type_code);
  val = valp->numval;
  min = minp != ((void *)0) ? *minp : cur->type.min;
  max = maxp != ((void *)0) ? *maxp : cur->type.max;
  if (tunable_val_lt (min, cur->type.min, unsigned_cmp))
    min = cur->type.min;
  if (tunable_val_gt (max, cur->type.max, unsigned_cmp))
    max = cur->type.max;
  if (tunable_val_gt (min, max, unsigned_cmp))
    {
      min = cur->type.min;
      max = cur->type.max;
    }
  if (tunable_val_lt (val, min, unsigned_cmp)
      || tunable_val_lt (max, val, unsigned_cmp))
    return;
  cur->val.numval = val;
  cur->type.min = min;
  cur->type.max = max;
  cur->initialized = 1;
}
static void
tunable_initialize (tunable_t *cur, const char *strval)
{
  tunable_val_t val;
  if (cur->type.type_code != TUNABLE_TYPE_STRING)
    val.numval = (tunable_num_t)_dl_strtoul (strval, ((void *)0));
  else
    val.strval = strval;
  do_tunable_update_val (cur, &val, ((void *)0), ((void *)0));
}
static void
parse_tunables (char *tunestr, char *valstring)
{
  if (tunestr == ((void *)0) || *tunestr == '\0')
    return;
  char *p = tunestr;
  size_t off = 0;
  while (1)
    {
      char *name = p;
      size_t len = 0;
      while (p[len] != '=' && p[len] != ':' && p[len] != '\0')
        len++;
      if (p[len] == '\0')
        {
          if (__libc_enable_secure)
            tunestr[off] = '\0';
          return;
        }
      if (p[len] == ':')
        {
          p += len + 1;
          continue;
        }
      p += len + 1;
      char *value = &valstring[p - tunestr];
      len = 0;
      while (p[len] != ':' && p[len] != '\0')
        len++;
      for (size_t i = 0; i < sizeof (tunable_list) / sizeof (tunable_t); i++)
        {
          tunable_t *cur = &tunable_list[i];
          if (tunable_is_name (cur->name, name))
            {
              if (__libc_enable_secure)
                {
                  if (cur->security_level != TUNABLE_SECLEVEL_SXID_ERASE)
                    {
                      if (off > 0)
                        tunestr[off++] = ':';
                      const char *n = cur->name;
                      while (*n != '\0')
                        tunestr[off++] = *n++;
                      tunestr[off++] = '=';
                      for (size_t j = 0; j < len; j++)
                        tunestr[off++] = value[j];
                    }
                  if (cur->security_level != TUNABLE_SECLEVEL_NONE)
                    break;
                }
              value[len] = '\0';
              tunable_initialize (cur, value);
              break;
            }
        }
      if (p[len] != '\0')
        p += len + 1;
    }
}
void
__tunables_init (char **envp)
{
  char *envname = ((void *)0);
  char *envval = ((void *)0);
  size_t len = 0;
  char **prev_envp = envp;
  while ((envp = get_next_env (envp, &envname, &len, &envval, &prev_envp))
         != ((void *)0))
    {
      if (tunable_is_name ("GLIBC_TUNABLES", envname))
        {
          char *new_env = tunables_strdup (envname);
          if (new_env != ((void *)0))
            parse_tunables (new_env + len + 1, envval);
          *prev_envp = new_env;
          continue;
        }
      for (int i = 0; i < sizeof (tunable_list) / sizeof (tunable_t); i++)
        {
          tunable_t *cur = &tunable_list[i];
          const char *name = cur->env_alias;
          if (tunable_is_name (name, envname))
            {
              tunable_initialize (cur, envval);
              break;
            }
        }
    }
}
