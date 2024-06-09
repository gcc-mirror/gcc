/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

typedef struct filter_list_entry {
  const char *name;
  int id;
  void (*function)();
} filter_list_entry;

static const filter_list_entry filter_list[9] = {0};

void php_zval_filter(int filter, int id1) {
  filter_list_entry filter_func;

  int size = 9;
  for (int i = 0; i < size; ++i) {
    if (filter_list[i].id == filter) {
      filter_func = filter_list[i];
      goto done;
    }
  }

#pragma GCC novector
  for (int i = 0; i < size; ++i) {
    if (filter_list[i].id == 0x0204) {
      filter_func = filter_list[i];
      goto done;
    }
  }
done:
  if (!filter_func.id)
    filter_func.function();
}
