/* Reduced from haproxy-2.7.1's cfgparse.c.  */

typedef __SIZE_TYPE__ size_t;

extern void*
calloc(size_t __nmemb, size_t __size)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__malloc__)) __attribute__((__alloc_size__(1, 2)));

struct list
{
  struct list* n;
  struct list* p;
};

struct cfg_postparser
{
  struct list list;
  char* name;
};

extern struct list postparsers;

int
test_1 (char* name)
{
  struct cfg_postparser* cp;

  cp = calloc(1, sizeof(*cp));
  if (!cp) {
    /* [...snip...] */
    return 0;
  }
  cp->name = name;

  (&cp->list)->p = (&postparsers)->p;
  (&postparsers)->p = (&cp->list);
  (&cp->list)->p->n = (&postparsers)->p;
  (&cp->list)->n = (&postparsers);

  return 1; /* { dg-bogus "leak of 'cp'" } */
}
