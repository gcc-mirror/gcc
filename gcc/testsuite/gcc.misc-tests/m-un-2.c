/* { dg-do compile } */
/* { dg-options "-W -Wall" } */

typedef unsigned long size_t;
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
}; /* { dg-warning "missing initializer" "warning regression" { target native } {18} } */
   /* { dg-warning "initialization for `mtable._realloc'" "warning regression" { target native } {18} } */

struct vtable mtable2 = {
  ._malloc = malloc,
  ._realloc = realloc
}; /* { dg-warning "missing initializer" "warning regression" { target native } {24} } */
   /* { dg-warning "initialization for `mtable2._free'" "warning regression" { target native } {24} } */

struct vtable mtable3 = {
  ._free = free,
  ._malloc = malloc,
  ._realloc = realloc
};
