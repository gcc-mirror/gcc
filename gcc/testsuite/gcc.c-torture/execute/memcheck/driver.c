/* GNU C dependencies:
   Checker support hooks
   ISO C 9x array element initialization
   void-pointer arithmetic */

#include "driver.h"

int verbose = 0;
int debug = 0;
int bad_accesses = 0;

const char *const memory_use_strings[] =
{
#define INIT(x)		[x] = #x
  INIT (MEMORY_USE_BAD),
  INIT (MEMORY_USE_DONT),
  INIT (MEMORY_USE_RO),
  INIT (MEMORY_USE_RW),
  INIT (MEMORY_USE_TW),
  INIT (MEMORY_USE_WO),
#undef INIT
};

/* This won't be used for any really huge test cases, so a simple
   linked list is adequate.  We won't even worry about overlapping
   regions; the matching entry that comes up first wins.  */
const char *const access_mode_strings[] =
{
  "none", "ro", "wo", "rw",
};
struct access_node
{
  struct access_node *next;
  const void *addr;
  size_t sz;
  enum access_mode mode;
};

static struct access_node *access_list;

void
mark_region (const void *addr, size_t sz, enum access_mode mode)
{
  struct access_node *a;
  if (debug)
    printf ("mark_region (%p, %ld, %s)\n", addr, (long) sz,
	    access_mode_strings[mode]);
  a = malloc (sizeof (struct access_node));
  a->next = access_list;
  a->addr = addr;
  a->sz = sz;
  a->mode = mode;
  access_list = a;
}

void report_bad_access (void *, size_t, enum memory_use_mode) NOCHECK;
void
report_bad_access (void *addr, size_t sz, enum memory_use_mode mode)
{
  if (++bad_accesses > 100)
    bad_accesses = 100;
  if (verbose)
    {
      static char x[100];
      const char *mode_str;
      if (mode >= 0
	  && mode < sizeof (memory_use_strings) / sizeof (*memory_use_strings)
	  && memory_use_strings[mode] != 0)
	mode_str = memory_use_strings[mode];
      else
	{
	  sprintf (x, "<bad mode %d>", mode);
	  mode_str = x;
	}
      printf ("bad access (%p, %ld, %s)\n", addr, (long) sz, mode_str);
    }
}

int verify1 (void *, size_t, enum access_mode, struct access_node *) NOCHECK;
int
verify1 (void *addr, size_t sz, enum access_mode mode,
	 struct access_node *a)
{
  while (a && (addr + sz <= a->addr || addr >= a->addr + a->sz))
    a = a->next;
  if (a == 0)
    return 0;

  if (debug)
    printf ("verify1 (%p, %ld, %s)\n", addr, (long) sz,
	    access_mode_strings[mode]);

  if (mode & ~a->mode)
    return 0;

  if (addr < a->addr)
    if (verify1 (a, a->addr - addr, mode, a->next) == 0)
      return 0;
  if (addr + sz > a->addr + a->sz)
    if (verify1 (a->addr + a->sz, (addr + sz) - (a->addr + a->sz), mode, a->next) == 0)
      return 0;

  /* All regions okay.  */
  return 1;
}

int verify_range_permission (void *, size_t, enum access_mode) NOCHECK;
int
verify_range_permission (void *addr, size_t sz, enum access_mode mode)
{
  if (debug)
    printf ("verify_range_permission (%p, %ld, %s)\n", addr, (long) sz,
	    access_mode_strings[mode]);
  return verify1 (addr, sz, mode, access_list);
}

void chkr_check_addr (void *, size_t, int) NOCHECK;
void
chkr_check_addr (void *addr, size_t sz, int mode)
{
  switch (mode)
    {
    case MEMORY_USE_BAD:
    case MEMORY_USE_DONT:
    default:
      report_bad_access (addr, sz, mode);
      return;
    case MEMORY_USE_RO:
      /* verify range readable */
      if (!verify_range_permission (addr, sz, ACCESS_RO))
	report_bad_access (addr, sz, mode);
      return;
    case MEMORY_USE_WO:
      /* verify writeable, set writeable and readable */
      if (!verify_range_permission (addr, sz, ACCESS_WO))
	report_bad_access (addr, sz, mode);
      mark_region (addr, sz, ACCESS_RW);
      return;
    case MEMORY_USE_RW:
      /* verify readable and writeable, no change */
      if (!verify_range_permission (addr, sz, ACCESS_RW))
	report_bad_access (addr, sz, mode);
      return;
    case MEMORY_USE_TW:
      /* verify writeable, no change */
      if (!verify_range_permission (addr, sz, ACCESS_WO))
	report_bad_access (addr, sz, mode);
      return;
    }
  /* All branches should return.  */
  abort ();
}

void copy1 (void *, void *, size_t, struct access_node *) NOCHECK;
void
copy1 (void *dest, void *src, size_t sz, struct access_node *a)
{
  while (a && (src + sz <= a->addr || src >= a->addr + a->sz))
    a = a->next;
  if (a == 0)
    {
      report_bad_access (src, sz, MEMORY_USE_RO);
      return;
    }

  if (debug)
    printf ("copy1 (%p, %p, %ld)\n", dest, src, (long) sz);

  {
    void *start, *end;
    start = src;
    if (start < a->addr)
      start = a->addr;
    end = src + sz;
    if (end > a->addr + a->sz)
      end = a->addr + a->sz;
    mark_region (dest + (start - src), end - start, a->mode);
  }

  if (src < a->addr)
    copy1 (dest, src, a->addr - src, a->next);
  if (src + sz > a->addr + a->sz)
    copy1 (dest + (a->addr + a->sz - src), a->addr + a->sz,
	   (src + sz) - (a->addr + a->sz), a->next);
}

void chkr_copy_bitmap (void *, void *, size_t) NOCHECK;
void
chkr_copy_bitmap (void *dest, void *src, size_t sz)
{
  if (verify_range_permission (dest, sz, MEMORY_USE_WO) == 0)
    report_bad_access (dest, sz, MEMORY_USE_WO);
  copy1 (dest, src, sz, access_list);
}

void chkr_set_right (void *, size_t, enum access_mode) NOCHECK;
void
chkr_set_right (void *addr, size_t sz, enum access_mode mode)
{
  mark_region (addr, sz, mode);
}

int main () NOCHECK;
int
main ()
{
  setup ();
  test ();
  bad_accesses = !!bad_accesses; /* get 0 or 1 */
  
  if (bad_accesses == expect_error)
    exit (0);
  else
    abort ();
  
  return 0;
}

struct malloc_node
{
  struct malloc_node *next;
  void *addr;
  size_t sz;
  unsigned is_free : 1;
};
static struct malloc_node *malloc_list;

void *
c_malloc (size_t sz)
{
  void *p;
  struct malloc_node *m;
  if (sz == 0)
    return 0;
  p = malloc (sz);
  if (p == 0)
    {
      if (verbose)
	printf ("malloc(%ld) failed\n", (long) sz);
      exit (1);
    }
  m = malloc (sizeof (struct malloc_node));
  if (m == 0)
    {
      if (verbose)
	printf ("malloc(%ld) failed\n", (long) sizeof (struct malloc_node));
      exit (1);
    }
  mark_region (p, sz, ACCESS_WO);
  m->addr = p;
  m->sz = sz;
  m->is_free = 0;
  m->next = malloc_list;
  malloc_list = m;
  return p;
}

void
c_free (void *p)
{
  struct malloc_node *m;
  if (p == 0)
    return;
  for (m = malloc_list; m; m = m->next)
    if (m->addr == p)
      break;
  if (m == 0 || m->is_free)
    /* Test is broken.  */
    abort ();
  m->is_free = 1;
  free (p);
}
