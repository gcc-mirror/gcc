#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

void *
hide_from_optimizer (const void *ptr)  __attribute__((noinline));

void *
hide_from_optimizer (const void *ptr)
{
  return (void *)ptr;
}

unsigned char global_buf[1024];

void
test_pointer_eq (unsigned *p, int i, int j)
{
  unsigned char local_declared_before_local_buf;
  unsigned char local_buf[16];
  unsigned char local_declared_after_local_buf;

  __analyzer_eval (p == hide_from_optimizer (p)); // { dg-warning "TRUE" }
  __analyzer_eval (local_buf == &local_buf[0]); // { dg-warning "TRUE" }

  /* Comparisons with NULL.  */
  __analyzer_eval (hide_from_optimizer (local_buf) == NULL); // { dg-warning "FALSE" }
  __analyzer_eval (hide_from_optimizer (NULL) == NULL); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (NULL) == p); // { dg-warning "UNKNOWN" }

  /* Same concrete base region.  */
  {
    // Same concrete offsets
    __analyzer_eval (local_buf == hide_from_optimizer (&local_buf[0])); // { dg-warning "TRUE" }

    // Different concrete offsets
    __analyzer_eval (local_buf == hide_from_optimizer (&local_buf[1])); // { dg-warning "FALSE" }

    // Same symbolic offset
    __analyzer_eval (&local_buf[i] == hide_from_optimizer (&local_buf[i])); // { dg-warning "TRUE" }

    // Possibly different symbolic offset
    {
      __analyzer_eval (&local_buf[i] == hide_from_optimizer (&local_buf[j])); // { dg-warning "UNKNOWN" }
      if (i == j)
	__analyzer_eval (&local_buf[i] == hide_from_optimizer (&local_buf[j])); // { dg-warning "TRUE" }
      else
	__analyzer_eval (&local_buf[i] == hide_from_optimizer (&local_buf[j])); // { dg-warning "FALSE" }
    }
  }

  /* Different concrete base regions.  */
  {
    __analyzer_eval (local_buf == hide_from_optimizer (global_buf)); // { dg-warning "FALSE" }

    // Within the buffer, we're definitely not pointing at other locals
    __analyzer_eval (&local_buf[0] == hide_from_optimizer (&local_declared_before_local_buf)); // { dg-warning "FALSE" }
    __analyzer_eval (&local_buf[15] == hide_from_optimizer (&local_declared_after_local_buf)); // { dg-warning "FALSE" }

    /* Outside the valid bounds of the buffer is undefined behavior.
       We currently happen to return FALSE for such attempts to form
       pointers to neighboring locals.  */
    __analyzer_eval (&local_buf[-1] == hide_from_optimizer (&local_declared_before_local_buf)); // { dg-warning "FALSE" }
    __analyzer_eval (&local_buf[16] == hide_from_optimizer (&local_declared_after_local_buf)); // { dg-warning "FALSE" }
  }

  /* Concrete vs symbolic where we know they're different
     (buf is local, so param p can't point to it).  */
  __analyzer_eval (local_buf == hide_from_optimizer (p)); // { dg-warning "FALSE" }

  /* Concrete vs symbolic where we don't know they're different
     (glob is global, so param p could point to it).  */
  __analyzer_eval (global_buf == hide_from_optimizer (p)); // { dg-warning "UNKNOWN" }

  // What about symbolic, could different offsets be the same?
  // TODO: what about overflow of locals, to try to alias (undefined behavior)
}

void
test_nonempty_heap_allocs (void)
{
  int i;

  // Known non-zero size
  void *p = __builtin_malloc (1024);
  void *q = __builtin_malloc (1024);

  __analyzer_eval (hide_from_optimizer (p) == p); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (p) == q); // { dg-warning "FALSE" }

  __analyzer_eval (hide_from_optimizer (p) == &i); // { dg-warning "FALSE" }
  __analyzer_eval (hide_from_optimizer (q) == &i); // { dg-warning "FALSE" }

  __builtin_free (p);
  __builtin_free (q);
}

void
test_maybe_empty_heap_allocs (size_t sz_p, size_t sz_q)
{
  int i;

  /* These could be zero-sized buffers, which some implementations
     return null for.  */
  void *p = __builtin_malloc (sz_p);
  void *q = __builtin_malloc (sz_q);

  __analyzer_eval (hide_from_optimizer (p) == p); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (p) == q); // { dg-warning "UNKNOWN" }

  __analyzer_eval (hide_from_optimizer (p) == &i); // { dg-warning "FALSE" }
  __analyzer_eval (hide_from_optimizer (q) == &i); // { dg-warning "FALSE" }

  __builtin_free (p);
  __builtin_free (q);
}

void
test_one_past_end (void)
{
  int arr[10];
  int *end = &arr[10];
  __analyzer_eval (hide_from_optimizer (end) == &arr[10]); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (end) == &arr[9]); // { dg-warning "FALSE" }
}

void
test_string_literals (void)
{
  const char *abc = "abc";
  const char *def = "def";
 
  __analyzer_eval (hide_from_optimizer (abc) == &abc[0]); // { dg-warning "TRUE" }

  __analyzer_eval (hide_from_optimizer (abc) == &abc[1]); // { dg-warning "FALSE" }
  
  __analyzer_eval (hide_from_optimizer (abc) == &def[0]); // { dg-warning "FALSE" "ideal" { xfail *-*-* } }
  // { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 }

  __analyzer_eval (hide_from_optimizer (abc) == &def[1]); // { dg-warning "FALSE" }
}

struct coord {int x; int y; };

void
test_struct_pointers (void)
{
  struct coord c1;
  struct coord c2;

  __analyzer_eval (hide_from_optimizer (&c1) == &c1.x); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (&c1) == &c1.y); // { dg-warning "FALSE" }

  __analyzer_eval (hide_from_optimizer (&c1) == &c2.x); // { dg-warning "FALSE" }
  __analyzer_eval (hide_from_optimizer (&c1) == &c2.y); // { dg-warning "FALSE" }  
}

union u {int x; char y; };

void
test_union_pointers (void)
{
  union u u1;
  union u u2;

  __analyzer_eval (hide_from_optimizer (&u1) == &u1.x); // { dg-warning "TRUE" }
  __analyzer_eval (hide_from_optimizer (&u1) == &u1.y); // { dg-warning "TRUE" }

  __analyzer_eval (hide_from_optimizer (&u1) == &u2.x); // { dg-warning "FALSE" }
  __analyzer_eval (hide_from_optimizer (&u1) == &u2.y); // { dg-warning "FALSE" }  
}
