/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-g -O -w" } */

typedef unsigned _GCC_ATTR_ALIGN_u32t;
typedef _GCC_ATTR_ALIGN_u32t _Uint32t __attribute__((__aligned__(4)));
typedef int _Intptrt __attribute__((__mode__(__pointer__)));
typedef unsigned int _Uintptrt __attribute__((__mode__(__pointer__)));
typedef _Intptrt ptrdiff_t;
typedef _Uintptrt uintptr_t;
typedef _Uint32t Elf32_Word;
typedef struct list_head list_head_t;

struct list_head {
  list_head_t *next;
};

struct object { };

struct objlist {
  struct object *object;
};

static uintptr_t ldd(void *frame) __attribute__((__used__));
static list_head_t *_dl_all_objects_ptr;
int dynvec(struct object *, int);
int relative_relocp(struct object *);
static void fini_array(struct object *const obj) {
  if ((dynvec(obj,(unsigned)26)) != ((Elf32_Word)-1)) {
    unsigned i;
    const unsigned funcs = (dynvec(obj,(unsigned)28)) / 4;
    const Elf32_Word fa = (dynvec(obj,(unsigned)26));
    const int *const p = (int *) relative_relocp(obj);
    for (i = (funcs ? (funcs - 1) : 0U); i != 0U; i--) {
      void (*const func)(void) = (void *) p[i];
      (*func)();
    }
  }
}
static void _do_exit_fini(void) {
  const struct objlist *o;
  for (((o)) = ((void *)(((list_head_t *)(_dl_all_objects_ptr))->next));
       !((((o))) == ((void *)((list_head_t *)(_dl_all_objects_ptr))));
       ((o)) = ((void *)(((list_head_t *)((o)))->next)))
    fini_array (o->object);
}
void atexit(void (*)(void));
static uintptr_t ldd (void *frame) {
  atexit(_do_exit_fini);
}
