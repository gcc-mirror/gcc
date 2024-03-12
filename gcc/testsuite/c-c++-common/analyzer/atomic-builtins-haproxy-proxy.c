/* Reduced from haproxy-2.7.1's proxy.c  */

/* { dg-require-effective-target sync_int_long_stack } */
/* { dg-require-effective-target sync_int_long } */

typedef __SIZE_TYPE__ size_t;

extern void* malloc(size_t __size)
  __attribute__((__nothrow__, __leaf__, __malloc__, __alloc_size__(1)));

extern void free(void* __ptr) __attribute__((__nothrow__, __leaf__));

struct error_snapshot
{
  /* [..snip...] */
};

struct proxy
{
  /* [..snip...] */
  struct error_snapshot *invalid_req, *invalid_rep;
  /* [..snip...] */
};

extern unsigned int error_snapshot_id;

void
proxy_capture_error(struct proxy* proxy,
		    int is_back)
{
  struct error_snapshot* es;
  unsigned int ev_id;

  /* [...snip...] */

  ev_id = __atomic_fetch_add(&error_snapshot_id, 1, 5);

  /* [...snip...] */

  es = (struct error_snapshot *) malloc(sizeof(*es));
  if (!es)
    return;

  /* [...snip...] */

  if (is_back) {
    es = __atomic_exchange_n(&proxy->invalid_rep, es, 4); /* { dg-bogus "leak" } */
  } else {
    es = __atomic_exchange_n(&proxy->invalid_req, es, 4); /* { dg-bogus "leak" } */
  }

  /* [...snip...] */

  free(es);
}
