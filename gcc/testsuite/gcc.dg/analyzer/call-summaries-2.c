/* { dg-additional-options "-fanalyzer-call-summaries --param analyzer-min-snodes-for-call-summary=0" } */
/* { dg-require-effective-target alloca } */

/* There need to be at least two calls to a function for the
   call-summarization code to be used.
   TODO: add some kind of test that summarization *was* used.  */

#include <stdlib.h>
#include "analyzer-decls.h"

extern int external_fn (void *);

int returns_const (void)
{
  return 42;
}

void test_summarized_returns_const (void)
{
  __analyzer_eval (returns_const () == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_const () == 42); /* { dg-warning "TRUE" } */
}

void test_summarized_returns_const_2 (void)
{
  returns_const (); /* { dg-message "when 'returns_const' returns" } */
  __analyzer_dump_path (); /* { dg-message "path" } */
}

int returns_param (int i)
{
  return i;
}

void test_summarized_returns_param (int j)
{
  __analyzer_eval (returns_param (j) == j); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_param (j) == j); /* { dg-warning "TRUE" } */
}

void writes_const_to_ptr (int *p)
{
  *p = 42;
}

void test_summarized_writes_const_to_ptr (void)
{
  int i, j;
  writes_const_to_ptr (&i);
  writes_const_to_ptr (&j);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */
}

// TODO: we should complain about this:

void test_summarized_write_through_null (void)
{
  writes_const_to_ptr (NULL);
}

void writes_param_to_ptr (int i, int *p)
{
  *p = i;
}

void test_summarized_writes_param_to_ptr (int j)
{
  int x, y;
  writes_param_to_ptr (j, &x);
  writes_param_to_ptr (j, &y);
  __analyzer_eval (x == j); /* { dg-warning "TRUE" } */
  __analyzer_eval (y == j); /* { dg-warning "TRUE" } */
}

void test_summarized_writes_param_to_ptr_unknown (int j)
{
  int *p = (int *)__analyzer_get_unknown_ptr ();
  writes_param_to_ptr (j, p);
  __analyzer_eval (*p == j); /* { dg-warning "UNKNOWN" } */
}

int g;

void writes_to_global (int i)
{
  g = i;
}

void test_writes_to_global (int x, int y)
{
  writes_to_global (x);
  __analyzer_eval (g == x); /* { dg-warning "TRUE" } */

  writes_to_global (y);
  __analyzer_eval (g == y); /* { dg-warning "TRUE" } */
}

int reads_from_global (void)
{
  return g;
}

void test_reads_from_global (int x, int y)
{
  g = x;
  __analyzer_eval (reads_from_global () == x); /* { dg-warning "TRUE" } */

  g = y;
  __analyzer_eval (reads_from_global () == y); /* { dg-warning "TRUE" } */
}

/* Example of a unary op.  */

int returns_negation (int i)
{
  return -i;
}

void test_returns_negation (int x)
{
  __analyzer_eval (returns_negation (5) == -5); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_negation (x) == -x); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_negation (-x) == x); /* { dg-warning "TRUE" } */
}

/* Example of a binary op.  */

int returns_sum (int i, int j)
{
  return i + j;
}

void test_returns_sum (int x, int y)
{
  __analyzer_eval (returns_sum (5, 3) == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_sum (7, 2) == 9); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_sum (x, y) == x + y); /* { dg-warning "TRUE" } */
}

struct coord
{
  int x;
  int y;
};

struct coord make_coord (int x, int y)
{
  struct coord result = {x, y};
  return result;
}

void test_make_coord (int i, int j)
{
  struct coord c1 = make_coord (3, 4);
  __analyzer_eval (c1.x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c1.y == 4); /* { dg-warning "TRUE" } */

  struct coord c2 = make_coord (i, j);
  __analyzer_eval (c2.x == i); /* { dg-warning "TRUE" } */
  __analyzer_eval (c2.y == j); /* { dg-warning "TRUE" } */
}

/* Example of nested usage of summaries.  */

struct rect
{
  struct coord nw;
  struct coord se;
};

struct rect make_rect (int top, int bottom, int left, int right)
{
  struct rect result = {make_coord (left, top),
			make_coord (right, bottom)};
  return result;
}

void test_make_rect (int top, int bottom, int left, int right)
{
  struct rect r1 = make_rect (3, 4, 5, 6);
  __analyzer_eval (r1.nw.y == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (r1.se.y == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (r1.nw.x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (r1.se.x == 6); /* { dg-warning "TRUE" } */

  struct rect r2 = make_rect (top, bottom, left, right);
  __analyzer_eval (r2.nw.y == top); /* { dg-warning "TRUE" } */
  __analyzer_eval (r2.se.y == bottom); /* { dg-warning "TRUE" } */
  __analyzer_eval (r2.nw.x == left); /* { dg-warning "TRUE" } */
  __analyzer_eval (r2.se.x == right); /* { dg-warning "TRUE" } */
}

const char *returns_str (void)
{
  return "abc";
}

void test_returns_str (void)
{
  __analyzer_eval (returns_str () != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_str ()[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_str ()[1] == 'b'); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_str ()[2] == 'c'); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_str ()[3] == '\0'); /* { dg-warning "TRUE" } */
}

int returns_field (struct coord *p)
{
  return p->y;
}

void test_returns_field (struct coord *q)
{
  __analyzer_eval (returns_field (q) == q->y); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_field (q) == q->y); /* { dg-warning "TRUE" } */
}

void writes_to_fields (struct coord *p, int x, int y)
{
  p->x = x;
  p->y = y;
}

void test_writes_to_field (struct coord *q, int qx, int qy)
{
  struct coord a, b;
  writes_to_fields (&a, 1, 2);
  __analyzer_eval (a.x == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (a.y == 2); /* { dg-warning "TRUE" } */
  writes_to_fields (&b, 3, 4);
  __analyzer_eval (b.x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (b.y == 4); /* { dg-warning "TRUE" } */
  writes_to_fields (q, qx, qy);
  __analyzer_eval (q->x == qx); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == qy); /* { dg-warning "TRUE" } */
}

/* Example of nested function summarization.  */

int get_min_x (struct rect *p)
{
  return p->nw.x;
}

int get_min_y (struct rect *p)
{
  return p->nw.y;
}

int get_max_x (struct rect *p)
{
  return p->se.x;
}

int get_max_y (struct rect *p)
{
  return p->se.y;
}

int get_area (struct rect *p)
{
  return ((get_max_x (p) - get_min_x (p))
	  * (get_max_y (p) - get_min_y (p)));
}

void test_get_area (int top, int bottom, int left, int right, struct rect *p)
{
  {
    /* 1x1 at origin.  */
    struct rect a = make_rect (0, 1, 0, 1);
    __analyzer_eval (get_min_y (&a) == 0); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_y (&a) == 1); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_min_x (&a) == 0); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_x (&a) == 1); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_area (&a) == 1); /* { dg-warning "TRUE" } */
  }

  {
    /* 4x5. */
    struct rect b = make_rect (3, 7, 4, 9);
    __analyzer_eval (get_min_y (&b) == 3); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_y (&b) == 7); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_min_x (&b) == 4); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_x (&b) == 9); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_area (&b) == 20); /* { dg-warning "TRUE" } */
  }

  {
    /* Symbolic.  */
    struct rect c = make_rect (top, bottom, left, right);
    __analyzer_eval (get_min_y (&c) == top); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_y (&c) == bottom); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_min_x (&c) == left); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_max_x (&c) == right); /* { dg-warning "TRUE" } */
    __analyzer_eval (get_area (&c) == ((right - left) * (bottom - top))); /* { dg-warning "TRUE" } */
  }

  /* Symbolic via ptr.  */
  __analyzer_eval (get_min_y (p) == p->nw.y); /* { dg-warning "TRUE" } */
  __analyzer_eval (get_max_y (p) == p->se.y); /* { dg-warning "TRUE" } */
  __analyzer_eval (get_min_x (p) == p->nw.x); /* { dg-warning "TRUE" } */
  __analyzer_eval (get_max_x (p) == p->se.x); /* { dg-warning "TRUE" } */
  __analyzer_eval (get_area (p) == ((p->se.x - p->nw.x) * (p->se.y - p->nw.y))); /* { dg-warning "TRUE" } */
}

int returns_element (int i)
{
  static const int arr[3] = {7, 8, 9};
  return arr[i];
}

void test_returns_element (int j)
{
  __analyzer_eval (returns_element (0) == 7); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_element (1) == 8); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_element (2) == 9); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_element (3) == 10); /* { dg-warning "UNKNOWN" } */
  // TODO: out of bounds
}

const int *returns_element_ptr (int i)
{
  static const int arr[3] = {7, 8, 9};
  return &arr[i];
}

int test_returns_element_ptr (int j)
{
  __analyzer_eval (*returns_element_ptr (0) == 7); /* { dg-warning "TRUE" } */
  __analyzer_eval (*returns_element_ptr (1) == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (*returns_element_ptr (2) == 9); /* { dg-warning "TRUE" } */
  return *returns_element_ptr (3); /* { dg-warning "buffer over-read" } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[2\\\]'" "valid subscript note" { target *-*-* } .-1 } */
}

int returns_offset (int arr[3], int i)
{
  return arr[i];
}

void test_returns_offset (int outer_arr[3], int idx)
{
  int a[3] = {4, 5, 6};
  __analyzer_eval (returns_offset (a, 0) == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset (a, 1) == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset (a, 2) == 6); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset (a, idx) == a[idx]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_offset (outer_arr, 0) == outer_arr[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset (outer_arr, idx) == outer_arr[idx]); /* { dg-warning "TRUE" } */  
}

int returns_offset_2 (int arr[], int i)
{
  return arr[i];
}

void test_returns_offset_2 (int outer_arr[], int idx)
{
  int a[3] = {4, 5, 6};
  __analyzer_eval (returns_offset_2 (a, 0) == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset_2 (a, 1) == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset_2 (a, 2) == 6); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset_2 (a, idx) == a[idx]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_offset_2 (outer_arr, 0) == outer_arr[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset_2 (outer_arr, idx) == outer_arr[idx]); /* { dg-warning "TRUE" } */  
}

int returns_offset_3 (int *p, int i)
{
  return p[i];
}

void test_returns_offset_3 (int *q, int j)
{
  __analyzer_eval (returns_offset_3 (q, j) == q[j]); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_offset_3 (q, j) == q[j]); /* { dg-warning "TRUE" } */
}

/* With state merging, this is summarized as returning "UNKNOWN".  */

int two_outcomes (int flag, int x, int y)
{
  if (flag)
    return x;
  else
    return y;
}

void test_two_outcomes (int outer_flag, int a, int b)
{
  int r;
  __analyzer_eval (two_outcomes (1, a, b) == a); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (two_outcomes (0, a, b) == b); /* { dg-warning "UNKNOWN" } */
  r = two_outcomes (outer_flag, a, b);
  if (outer_flag)
    __analyzer_eval (r == a); /* { dg-warning "UNKNOWN" } */
  else
    __analyzer_eval (r == b); /* { dg-warning "UNKNOWN" } */  
}

/* Verify that summary replays capture postconditions.  */

void check_int_nonnegative (int i)
{
  if (i < 0)
    __builtin_unreachable ();
}

void test_check_int_nonnegative (int i, int j)
{
  __analyzer_eval (i >= 0); /* { dg-warning "UNKNOWN" } */
  check_int_nonnegative (i);
  __analyzer_eval (i >= 0); /* { dg-warning "TRUE" } */

  __analyzer_eval (j >= 0); /* { dg-warning "UNKNOWN" } */
  check_int_nonnegative (j);
  __analyzer_eval (j >= 0); /* { dg-warning "TRUE" } */
}

void calls_external_fn (void)
{
  external_fn (NULL);
}

void test_calls_external_fn (void)
{
  g = 1;
  __analyzer_eval (g == 1); /* { dg-warning "TRUE" } */
  calls_external_fn ();
  calls_external_fn ();
  __analyzer_eval (g == 1); /* { dg-warning "UNKNOWN" "expected" { xfail *-*-* } } */
  /* { dg-bogus "TRUE" "bogus" { xfail *-*-* } .-1 } */
  // TODO(xfails)
}

int returns_iterator (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
    }
  return i;
}

void test_returns_iterator (int j, int k)
{
  __analyzer_eval (returns_iterator (j) == j); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_iterator (k) == k); /* { dg-warning "UNKNOWN" } */
  /* TODO: ideally we'd capture these equalities, but this is an issue 
     with how we handle loops.  */
}

int returns_external_result (void)
{
  return external_fn (NULL);
}

int test_returns_external_result (void)
{
  int i, j;
  i = returns_external_result ();
  j = returns_external_result ();
  __analyzer_eval (i == j); /* { dg-warning "UNKNOWN" } */
  return i * j;
}

int uses_alloca (int i)
{
  int *p = __builtin_alloca (sizeof (int));
  *p = i;
  return *p;
}

void test_uses_alloca (int x)
{
  __analyzer_eval (uses_alloca (42) == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (uses_alloca (x) == x); /* { dg-warning "TRUE" } */
}

struct bits
{
  unsigned b0 : 1;
  unsigned b1 : 1;
  unsigned b2 : 1;
  unsigned b3 : 1;
  unsigned b4 : 1;
  unsigned b5 : 1;
  unsigned b6 : 1;
  unsigned b7 : 1;
};

int returns_bitfield (struct bits b)
{
  return b.b3;
}

void test_returns_bitfield (struct bits s)
{
  __analyzer_eval (returns_bitfield (s) == s.b3); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_bitfield (s) == s.b3); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally it would figure out that these are equal
}

int consume_two_ints_from_va_list (__builtin_va_list ap)
{
  int i, j;
  i = __builtin_va_arg (ap, int);
  j = __builtin_va_arg (ap, int);
  return i * j;
}

int test_consume_two_ints_from_va_list (__builtin_va_list ap1)
{
  int p1, p2;
  __builtin_va_list ap2;
  __builtin_va_copy (ap2, ap1);
  p1 = consume_two_ints_from_va_list (ap1);
  p2 = consume_two_ints_from_va_list (ap2);
  __analyzer_eval (p1 == p2); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally it would figure out these are equal
  __builtin_va_end (ap2);
}

int consume_two_ints_from_varargs (int placeholder, ...)
{
  int i, j;
  __builtin_va_list ap;
  __builtin_va_start (ap, placeholder);
  i = __builtin_va_arg (ap, int);
  j = __builtin_va_arg (ap, int);
  __builtin_va_end (ap);
  return i * j;
}

void test_consume_two_ints_from_varargs (int x, int y)
{
  __analyzer_eval (consume_two_ints_from_varargs (0, 4, 5) == 20); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (consume_two_ints_from_varargs (0, 3, 6) == 18); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (consume_two_ints_from_varargs (0, x, 6) == x * y); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally it would figure these out
}

extern int const_fn_1 (int) __attribute__ ((const));
int calls_const_fn (int i)
{
  return const_fn_1 (i);
}

void test_calls_const_fn (int x)
{
  int r1, r2;
  r1 = calls_const_fn (x);
  r2 = calls_const_fn (x);
  __analyzer_eval (r1 == r2); /* { dg-warning "TRUE" } */
}

typedef struct iv2 { int arr[2]; } iv2_t;
typedef struct iv4 { int arr[4]; } iv4_t;

iv2_t returns_iv2_t (int x, int y)
{
  iv2_t result = {x, y};
  return result;
}

void test_returns_iv2_t (int a, int b)
{
  __analyzer_eval (returns_iv2_t (a, b).arr[0] == a); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_iv2_t (a, b).arr[1] == b); /* { dg-warning "TRUE" } */
}

iv4_t returns_iv4_t (int a, iv2_t bc, int d)
{
  iv4_t result = {a, bc.arr[0], bc.arr[1], d};
  return result;
}

void test_returns_iv4_t (int p, iv2_t qr, int s)
{
  __analyzer_eval (returns_iv4_t (p, qr, s).arr[0] == p); /* { dg-warning "TRUE" } */
  __analyzer_eval (returns_iv4_t (p, qr, s).arr[1] == qr.arr[0]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_iv4_t (p, qr, s).arr[2] == qr.arr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_iv4_t (p, qr, s).arr[3] == s); /* { dg-warning "TRUE" } */
  // TODO: ideally the UNKNOWNs would be TRUEs.
}

void copies_iv2_t (int *p, iv2_t xy)
{
  __builtin_memcpy (p, &xy, sizeof (xy));
}

void test_copies_iv2_t (iv2_t ab, iv2_t cd)
{
  iv4_t t;
  copies_iv2_t (&t.arr[0], ab);
  copies_iv2_t (&t.arr[2], cd);
  __analyzer_eval (t.arr[0] = ab.arr[0]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (t.arr[1] = ab.arr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (t.arr[2] = cd.arr[0]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (t.arr[3] = cd.arr[1]); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally the UNKNOWNs would be TRUEs.
}

void partially_inits (int *p, int v)
{
  p[1] = v;
}

void test_partially_inits_0 (int x)
{
  int arr[2];
  partially_inits (arr, x);
  partially_inits (arr, x);

  __analyzer_eval (arr[0]); /* { dg-warning "use of uninitialized value 'arr\\\[0\\\]'" } */
}

void test_partially_inits_1 (int x)
{
  int arr[2];
  partially_inits (arr, x);
  partially_inits (arr, x);

  __analyzer_eval (arr[1] == x); /* { dg-bogus "use of uninitialized value 'arr\\\[1\\\]'" "uninit" { xfail *-*-* } } */
  // TODO(xfail), and eval should be "TRUE"
}

int uses_switch (int i)
{
  switch (i)
    {
    case 0:
      return 42;
    case 1:
      return 17;
    default:
      return i * 2;
    }
}

void test_uses_switch (int x)
{
  __analyzer_eval (uses_switch (0) == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (uses_switch (1) == 17); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (uses_switch (2) == x * 2); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally the UNKNOWNs would be TRUEs.
}

int *returns_ptr_to_first_field (struct coord *p)
{
  return &p->x;
}

void test_returns_ptr_to_first_field (struct coord *q)
{
  __analyzer_eval (returns_ptr_to_first_field (q) == (int *)q); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (returns_ptr_to_first_field (q) == (int *)q); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally the UNKNOWNs would be TRUEs.
}
