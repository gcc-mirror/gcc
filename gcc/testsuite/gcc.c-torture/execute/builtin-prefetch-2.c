/* Test that __builtin_prefetch does no harm.

   Prefetch data using a variety of storage classes and address
   expressions.  */

void exit (int);

int glob_int_arr[100];
int *glob_ptr_int = glob_int_arr;
int glob_int = 4;

static int stat_int_arr[100];
static int *stat_ptr_int = stat_int_arr;
static int stat_int;

struct S {
  int a;
  short b, c;
  char d[8];
  struct S *next;
};

struct S str;
struct S *ptr_str = &str;

/* Prefetch global variables using the address of the variable.  */

void
simple_global ()
{
  __builtin_prefetch (glob_int_arr, 0, 0);
  __builtin_prefetch (glob_ptr_int, 0, 0);
  __builtin_prefetch (&glob_int, 0, 0);
}

/* Prefetch file-level static variables using the address of the variable.  */

void
simple_file ()
{
  __builtin_prefetch (stat_int_arr, 0, 0);
  __builtin_prefetch (stat_ptr_int, 0, 0);
  __builtin_prefetch (&stat_int, 0, 0);
}

/* Prefetch local static variables using the address of the variable.  */

void
simple_static_local ()
{
  static int gx[100];
  static int *hx = gx;
  static int ix;
  __builtin_prefetch (gx, 0, 0);
  __builtin_prefetch (hx, 0, 0);
  __builtin_prefetch (&ix, 0, 0);
}

/* Prefetch local stack variables using the address of the variable.  */

void
simple_local ()
{
  int gx[100];
  int *hx = gx;
  int ix;
  __builtin_prefetch (gx, 0, 0);
  __builtin_prefetch (hx, 0, 0);
  __builtin_prefetch (&ix, 0, 0);
}

/* Prefetch arguments using the address of the variable.  */

void
simple_arg (int g[100], int *h, int i)
{
  __builtin_prefetch (g, 0, 0);
  __builtin_prefetch (h, 0, 0);
  __builtin_prefetch (&i, 0, 0);
}

/* Prefetch using address expressions involving global variables.  */

void
expr_global (void)
{
  __builtin_prefetch (&str, 0, 0);
  __builtin_prefetch (ptr_str, 0, 0);
  __builtin_prefetch (&str.b, 0, 0);
  __builtin_prefetch (&ptr_str->b, 0, 0);
  __builtin_prefetch (&str.d, 0, 0);
  __builtin_prefetch (&ptr_str->d, 0, 0);
  __builtin_prefetch (str.next, 0, 0);
  __builtin_prefetch (ptr_str->next, 0, 0);
  __builtin_prefetch (str.next->d, 0, 0);
  __builtin_prefetch (ptr_str->next->d, 0, 0);

  __builtin_prefetch (&glob_int_arr, 0, 0);
  __builtin_prefetch (glob_ptr_int, 0, 0);
  __builtin_prefetch (&glob_int_arr[2], 0, 0);
  __builtin_prefetch (&glob_ptr_int[3], 0, 0);
  __builtin_prefetch (glob_int_arr+3, 0, 0);
  __builtin_prefetch (glob_int_arr+glob_int, 0, 0);
  __builtin_prefetch (glob_ptr_int+5, 0, 0);
  __builtin_prefetch (glob_ptr_int+glob_int, 0, 0);
}

/* Prefetch using address expressions involving local variables.  */

void
expr_local (void)
{
  int b[10];
  int *pb = b;
  struct S t;
  struct S *pt = &t;
  int j = 4;

  __builtin_prefetch (&t, 0, 0);
  __builtin_prefetch (pt, 0, 0);
  __builtin_prefetch (&t.b, 0, 0);
  __builtin_prefetch (&pt->b, 0, 0);
  __builtin_prefetch (&t.d, 0, 0);
  __builtin_prefetch (&pt->d, 0, 0);
  __builtin_prefetch (t.next, 0, 0);
  __builtin_prefetch (pt->next, 0, 0);
  __builtin_prefetch (t.next->d, 0, 0);
  __builtin_prefetch (pt->next->d, 0, 0);

  __builtin_prefetch (&b, 0, 0);
  __builtin_prefetch (pb, 0, 0);
  __builtin_prefetch (&b[2], 0, 0);
  __builtin_prefetch (&pb[3], 0, 0);
  __builtin_prefetch (b+3, 0, 0);
  __builtin_prefetch (b+j, 0, 0);
  __builtin_prefetch (pb+5, 0, 0);
  __builtin_prefetch (pb+j, 0, 0);
}

int
main ()
{
  simple_global ();
  simple_file ();
  simple_static_local ();
  simple_local ();
  simple_arg (glob_int_arr, glob_ptr_int, glob_int);

  str.next = &str;
  expr_global ();
  expr_local ();

  exit (0);
}
