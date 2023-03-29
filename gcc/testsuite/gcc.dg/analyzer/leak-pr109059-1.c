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
  int (*func)();
};

extern struct list postparsers;

int
cfg_register_postparser(char* name, int (*func)())
{
  struct cfg_postparser* cp;

  cp = calloc(1, sizeof(*cp));
  if (!cp) {
    /* [...snip...] */
    return 0;
  }
  cp->name = name;
  cp->func = func;

  ({
    (&cp->list)->p = (&postparsers)->p;
    (&cp->list)->p->n = (&postparsers)->p = (&cp->list);
    (&cp->list)->n = (&postparsers);
    (&cp->list);
  });

  return 1; /* { dg-bogus "leak of 'cp'" } */
}
