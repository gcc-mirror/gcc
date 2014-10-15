/* { dg-do compile } */
/* { dg-options "-O2" } */
typedef long unsigned int size_t;
typedef long int intmax_t;
typedef long unsigned int uintmax_t;
extern void *xmalloc (size_t) __attribute__ ((__malloc__));
extern const char *trim_filename (const char *);
extern void error (const char *, ...);

static __inline void *
__inline_memcpy_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}

typedef unsigned int hashval_t;
typedef hashval_t (*htab_hash) (const void *);
typedef int (*htab_eq) (const void *, const void *);
typedef void (*htab_del) (void *);
typedef int (*htab_trav) (void **, void *);
typedef void *(*htab_alloc) (size_t, size_t);
typedef void (*htab_free) (void *);

typedef void *(*htab_alloc_with_arg) (void *, size_t, size_t);
typedef void (*htab_free_with_arg) (void *, void *);
struct htab {
  htab_hash hash_f;
  htab_eq eq_f;
  htab_del del_f;
  void ** entries;
  size_t size;
  size_t n_elements;
  size_t n_deleted;
  unsigned int searches;
  unsigned int collisions;
  htab_alloc alloc_f;
  htab_free free_f;
  void * alloc_arg;
  htab_alloc_with_arg alloc_with_arg_f;
  htab_free_with_arg free_with_arg_f;
  unsigned int size_prime_index;
};

typedef struct htab *htab_t;
enum insert_option {NO_INSERT, INSERT};
extern void * htab_find (htab_t, const void *);
extern void ** htab_find_slot (htab_t, const void *, enum insert_option);

enum mode_class { MODE_RANDOM, MODE_CC, MODE_INT, MODE_PARTIAL_INT, MODE_FRACT, MODE_UFRACT, MODE_ACCUM, MODE_UACCUM, MODE_FLOAT, MODE_DECIMAL_FLOAT, MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT, MODE_VECTOR_INT, MODE_VECTOR_FRACT, MODE_VECTOR_UFRACT, MODE_VECTOR_ACCUM, MODE_VECTOR_UACCUM, MODE_VECTOR_FLOAT, MAX_MODE_CLASS };

static const char *const mode_class_names[MAX_MODE_CLASS] =
{
  "MODE_RANDOM", "MODE_CC", "MODE_INT", "MODE_PARTIAL_INT", "MODE_FRACT", "MODE_UFRACT", "MODE_ACCUM", "MODE_UACCUM", "MODE_FLOAT", "MODE_DECIMAL_FLOAT", "MODE_COMPLEX_INT", "MODE_COMPLEX_FLOAT", "MODE_VECTOR_INT", "MODE_VECTOR_FRACT", "MODE_VECTOR_UFRACT", "MODE_VECTOR_ACCUM", "MODE_VECTOR_UACCUM", "MODE_VECTOR_FLOAT"
};
struct mode_data
{
  struct mode_data *next;

  const char *name;
  enum mode_class cl;
  unsigned int precision;
  unsigned int bytesize;
  unsigned int ncomponents;
  unsigned int alignment;
  const char *format;

  struct mode_data *component;
  struct mode_data *wider;
  struct mode_data *wider_2x;

  struct mode_data *contained;

  struct mode_data *next_cont;

  const char *file;
  unsigned int line;
  unsigned int counter;
  unsigned int ibit;
  unsigned int fbit;
};

static struct mode_data *modes[MAX_MODE_CLASS];
static unsigned int n_modes[MAX_MODE_CLASS];
static struct mode_data *void_mode;

static const struct mode_data blank_mode = {
  0, "<unknown>", MAX_MODE_CLASS,
  -1U, -1U, -1U, -1U,
  0, 0, 0, 0, 0, 0,
  "<unknown>", 0, 0, 0, 0
};

static htab_t modes_by_name;

static __inline__ struct mode_data *
find_mode (const char *name)
{
  struct mode_data key;

  key.name = name;
  return (struct mode_data *) htab_find (modes_by_name, &key);
}

static struct mode_data *
new_mode (enum mode_class cl, const char *name,
   const char *file, unsigned int line)
{
  struct mode_data *m;
  static unsigned int count = 0;

  m = find_mode (name);
  if (m)
    {
      error ("%s:%d: duplicate definition of mode \"%s\"",
      trim_filename (file), line, name);
      error ("%s:%d: previous definition here", m->file, m->line);
      return m;
    }

  m = ((struct mode_data *) xmalloc (sizeof (struct mode_data)));
  ((__builtin_object_size (m, 0) != (size_t) -1) ? __builtin___memcpy_chk (m, &blank_mode, sizeof (struct mode_data), __builtin_object_size (m, 0)) : __inline_memcpy_chk (m, &blank_mode, sizeof (struct mode_data)));
  m->cl = cl;
  m->name = name;
  if (file)
    m->file = trim_filename (file);
  m->line = line;
  m->counter = count++;

  m->next = modes[cl];
  modes[cl] = m;
  n_modes[cl]++;

  *htab_find_slot (modes_by_name, m, INSERT) = m;

  return m;
}

static void
make_int_mode (const char *name,
        unsigned int precision, unsigned int bytesize,
        const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_INT, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
}

static void
create_modes (void)
{
make_int_mode ("HI", -1U, 2, "../../work/gcc/machmode.def", 182);
}

int
main (int argc, char **argv)
{
  create_modes ();
}
