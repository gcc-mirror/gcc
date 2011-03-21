/* { dg-do compile } */

typedef long unsigned int size_t;

static __inline void *
__inline_memcpy_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}

extern void *xmalloc (size_t) __attribute__ ((__malloc__));

struct htab { void ** entries; };

typedef struct htab *htab_t;

extern void ** htab_find_slot (htab_t, const void *);

enum mode_class { MODE_RANDOM, MODE_CC, MODE_INT, MAX_MODE_CLASS };

struct mode_data
{
  struct mode_data *next;
  enum mode_class cl;
};

static const struct mode_data blank_mode = { 0, MAX_MODE_CLASS };

static htab_t modes_by_name;

struct mode_data *
new_mode (void)
{
  struct mode_data *m
    = ((struct mode_data *) xmalloc (sizeof (struct mode_data)));

  ((__builtin_object_size (m, 0) != (size_t) -1) ? __builtin___memcpy_chk (m, &blank_mode, sizeof (struct mode_data), __builtin_object_size (m, 0)) : __inline_memcpy_chk (m, &blank_mode, sizeof (struct mode_data)));

  *htab_find_slot (modes_by_name, m) = m;

  return m;
}
