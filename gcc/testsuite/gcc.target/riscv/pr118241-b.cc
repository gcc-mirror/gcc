/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64imafdc_zba_zbb_zbs_zicbom_zicbop -mabi=lp64d" } */

/* Reduced from libsanitizer::asan_allocator.  */

enum a { c };
class d;
struct e {
  long count;
  void *batch[];
};
template <typename> class f {
public:
  void g() {
    if (e *b = h->i())
      for (; b->count;)
        if (6 < b->count)
          __builtin_prefetch(b->batch[6]);
  }
  d *h;
};
class d {
public:
  e *i();
};
struct j {
  f<int> k;
  j(a);
  void l() { k.g(); }
} a(c);
void m() { a.l(); }

/* { dg-final { scan-assembler-times "prefetch.r\t0\\(\[a-x0-9\]+\\)" 1 } } */
