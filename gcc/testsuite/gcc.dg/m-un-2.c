/* { dg-do compile } */
/* { dg-options "-W -Wall" } */

typedef __SIZE_TYPE__ size_t;
extern void* malloc (size_t);
extern void free (void*);
extern void* realloc (void*, size_t);

struct vtable {
  void* (* _malloc) (size_t);
  void (* _free) (void*);
  void* (* _realloc) (void*, size_t);
};

struct vtable mtable = {
  malloc,
  free
}; /* { dg-warning "missing initializer" "warning regression" { target *-*-* } {18} } */
   /* { dg-warning "initialization for 'mtable._realloc'" "warning regression 2" { target *-*-* } {18} } */

/* With designated initializers, we assume you meant to leave out the
   initialization of any blank fields.  */
struct vtable mtable2 = {
  ._malloc = malloc,
  ._realloc = realloc
};

struct vtable mtable3 = {
  ._free = free,
  ._malloc = malloc,
  ._realloc = realloc
};
