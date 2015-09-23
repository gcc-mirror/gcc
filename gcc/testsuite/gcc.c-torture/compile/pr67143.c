long a, c;
int b;
int d;
void ut_dbg_assertion_failed() __attribute__((noreturn));
long dict_index_is_spatial(int *);
void btr_block_get_func(char *);
long btr_page_get_level_low(unsigned char *);
void btr_validate_level(long p1) {
  unsigned char *e;
  while (p1 != btr_page_get_level_low(e)) {
    if (__builtin_expect(b, 0))
      ut_dbg_assertion_failed();
    if (dict_index_is_spatial(&d))
      while (c != 5535) {
        __sync_add_and_fetch(&a, 536870912);
        btr_block_get_func("");
      }
  }
  for (long i; i; ++i)
    btr_validate_level(-i);
}
