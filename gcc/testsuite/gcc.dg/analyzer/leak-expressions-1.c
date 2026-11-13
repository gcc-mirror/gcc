/* Verify that we issue readable expressions in leak reports.
   C only, since C++ adds various "class::" prefixes.  */

struct s2 { void *m_arr[10]; };
struct s5 { int m_pad; void *m_arr[10]; };
union u1 { void *m_ptr; int m_i; };
struct s6 { void *m_a; void *m_b; };
struct s7 { struct s2 m_inner[3]; };

void
test_array_of_array_cc (void)
{
  void *arr[5][10];
  arr[3][4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\]\\\[4\\\]'" } */

void
test_array_of_array_0c (void)
{
  void *arr[5][10];
  arr[0][4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[0\\\]\\\[4\\\]'" } */

void
test_array_of_array_c0 (void)
{
  void *arr[5][10];
  arr[3][0] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\]\\\[0\\\]'" } */

void
test_array_of_array_cs (int j)
{
  void *arr[5][10];
  arr[3][j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\]\\\[j\\\]'" } */

void
test_array_of_array_0s (int j)
{
  void *arr[5][10];
  arr[0][j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[0\\\]\\\[j\\\]'" } */

void
test_array_of_array_sc (int i)
{
  void *arr[5][10];
  arr[i][5] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\]\\\[5\\\]'" } */

void
test_array_of_array_ss (int i, int j)
{
  void *arr[5][10];
  arr[i][j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\]\\\[j\\\]'" } */

void
test_pad_cc (void)
{
  struct s5 arr[5];
  arr[3].m_arr[4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[4\\\]'" } */

void
test_pad_sc (int i)
{
  struct s5 arr[5];
  arr[i].m_arr[4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_arr\\\[4\\\]'" } */

void
test_pad_cs (int j)
{
  struct s5 arr[5];
  arr[3].m_arr[j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[j\\\]'" } */

void
test_pad_ss (int i, int j)
{
  struct s5 arr[5];
  arr[i].m_arr[j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_arr\\\[j\\\]'" } */

void
test_same_index_at_both_levels (int i)
{
  struct s2 arr[5];
  arr[i].m_arr[i] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_arr\\\[i\\\]'" "ideal" { xfail *-*-* } } */
/* { dg-bogus "leak of '<unknown>'" "status quo" { xfail *-*-* } .-1 } */

void
test_scaled_index (int i, int j)
{
  struct s2 arr[5];
  arr[2*i].m_arr[j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i \\\* 2\\\].m_arr\\\[j\\\]'" } */

void
test_index_plus_constant_offset (int i, int j)
{
  struct s2 arr[5];
  arr[i+1].m_arr[j] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i \\\+ 1\\\].m_arr\\\[j\\\]'" } */

void
test_scaled_sc (int i)
{
  struct s2 arr[5];
  arr[2*i].m_arr[4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i \\\* 2\\\].m_arr\\\[4\\\]'" } */

void
test_scaled_cs (int i)
{
  struct s2 arr[5];
  arr[3].m_arr[2*i] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[i \\\* 2\\\]'" } */

void
test_plus_sc (int i)
{
  struct s2 arr[5];
  arr[i+1].m_arr[4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i \\\+ 1\\\].m_arr\\\[4\\\]'" } */

void
test_plus_cs (int i)
{
  struct s2 arr[5];
  arr[3].m_arr[i+1] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[i \\\+ 1\\\]'" } */

void
test_1d_ptr_array (int i)
{
  void *arr[10];
  arr[i] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\]'" } */

void
test_union (int i)
{
  union u1 arr[5];
  arr[i].m_ptr = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_ptr'" } */

void
test_field (int i)
{
  struct s6 arr[5];
  arr[i].m_b = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_b'" } */

void
test_three_levels_sss (int i, int j, int k)
{
  struct s7 arr[5];
  arr[i].m_inner[j].m_arr[k] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\].m_inner\\\[j\\\].m_arr\\\[k\\\]'" } */

void
test_three_levels_csc (int i, int j, int k)
{
  struct s7 arr[5];
  arr[2].m_inner[j].m_arr[4] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[2\\\].m_inner\\\[j\\\].m_arr\\\[4\\\]'" } */

void
test_three_levels_cs0 (int i, int j, int k)
{
  struct s7 arr[5];
  arr[2].m_inner[j].m_arr[0] = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[2\\\].m_inner\\\[j\\\].m_arr\\\[0\\\]'" } */

void
test_ptrarith (int i)
{
  void *arr[10];
  *(void **)((char *)arr + i * sizeof (void *)) = __builtin_malloc (1024);
} /* { dg-warning "leak of 'arr\\\[i\\\]'" "ideal" { xfail *-*-* } } */
/* { dg-bogus "leak of '<unknown>'" "status quo" { xfail *-*-* } .-1 } */
