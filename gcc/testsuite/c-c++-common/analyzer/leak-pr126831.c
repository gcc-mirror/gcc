/* Verify that we don't emit a false positive about a leak of "r.buf"
   when returning r by value when passing a pointer to a field
   to an external function before the malloc (PR analyzer/126831).
   Reduced from lz4-1.10.0 LZ4IO_createDResources().  */

extern int
setup (void *);

typedef struct {
  void *buf;
  void *ctx;
} res_t;

res_t
test_valid (void)
{
  res_t r;
  setup(&r.ctx);
  r.buf = __builtin_malloc (100);
  return r;
} /* { dg-bogus "leak" "PR analyzer/126831 leak false +ve" } */

void
test_leak_due_to_missing_return (void)
{
  res_t r;
  setup(&r.ctx);
  r.buf = __builtin_malloc (100);
} /* { dg-warning "leak" } */
