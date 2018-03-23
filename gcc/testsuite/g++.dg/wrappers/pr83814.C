/* Verify that our memset warnings don't crash when folding
   arguments within a template (PR c++/83814).  */

// { dg-options "-Wno-int-to-pointer-cast -Wmemset-transposed-args -Wmemset-elt-size" }

template <class>
void test_1()
{
  __builtin_memset (int() - char(), 0, 0);
}

template <class>
void test_2()
{
  __builtin_memset (0, 0, int() - char());
}

template <class>
void test_3 (unsigned a, int c)
{
  __builtin_memset((char *)c + a, 0, a);
}

template <class>
void test_4 (unsigned a, int c)
{
  __builtin_memset(0, 0, (char *)c + a);
}

/* Verify that we warn for -Wmemset-transposed-args inside
   a template.  */

char buf[1024];

template <class>
void test_5 ()
{
  __builtin_memset (buf, sizeof buf, 0); // { dg-warning "transposed parameters" }
}

/* Adapted from c-c++-common/memset-array.c; verify that 
   -Wmemset-elt-size works within a template.  */

enum a {
  a_1,
  a_2,
  a_n
};
int t1[20];
int t2[a_n];

struct s
{
  int t[20];
};

template<class>
void foo (struct s *s)
{
  __builtin_memset (t1, 0, 20); // { dg-warning "element size" }

  // This case requires reading through an enum value:
  __builtin_memset (t2, 0, a_n); // { dg-warning "element size" }

  __builtin_memset (s->t, 0, 20); // { dg-warning "element size" }

  // These cases require folding of arg2 within a template:
  __builtin_memset (t2, 0, a_n + 0); // { dg-warning "element size" }
  __builtin_memset (t2, 0, a_n * 1); // { dg-warning "element size" }
}
