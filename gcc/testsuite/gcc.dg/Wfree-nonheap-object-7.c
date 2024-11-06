/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -Wfree-nonheap-object" } */

struct local_caches *get_local_caches_lcs;
void *calloc(long, long);
void *realloc();

struct local_caches {
  int *t_mem_caches;
};

struct local_caches *get_local_caches() {
  if (get_local_caches_lcs)
    return get_local_caches_lcs;
  get_local_caches_lcs = calloc(1, 0);
  return get_local_caches_lcs;
}

void libtrace_ocache_free() {
  struct local_caches lcs = *get_local_caches(), __trans_tmp_1 = lcs;
  {
    struct local_caches *lcs = &__trans_tmp_1;
    lcs->t_mem_caches += 10;
    __trans_tmp_1.t_mem_caches = realloc(__trans_tmp_1.t_mem_caches, sizeof(int)); // { dg-warning "called on pointer (?:(?!PHI).)*nonzero offset" }
  }
}
