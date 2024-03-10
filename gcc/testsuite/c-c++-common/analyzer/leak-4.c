/* Various tests of memory leak detection.  */

#include <stdlib.h>

/* Example of an leak due to incomplete cleanup when freeing a struct.  */

struct s1
{
  void *ptr;
};

void test_1 (void)
{
  struct s1 *a = (struct s1 *) malloc (sizeof (struct s1));
  if (!a)
    return;
  a->ptr = malloc (1024); /* { dg-message "allocated here" } */
  free (a); /* { dg-warning "leak of '<unknown>'" } */
  /* TODO: we should print "a->ptr' here, rather than '<unknown>'
     (PR analyzer/99771).  */
}


/* Examples involving arrays.  */

struct s2
{
  void *m_arr[10];
};

void test_2a (void)
{
  struct s2 arr[5];
  arr[3].m_arr[4] = malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[4\\\]'" "" { target c } } */
/* { dg-warning "leak of 'arr\\\[3\\\].s2::m_arr\\\[4\\\]'" "" { target c++ } .-1 } */

void test_2b (int i)
{
  struct s2 arr[5];
  arr[3].m_arr[i] = malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'arr\\\[3\\\].m_arr\\\[i\\\]'" "" { target c } } */
/* { dg-warning "leak of 'arr\\\[3\\\].s2::m_arr\\\[i\\\]'" "" { target c++ } .-1 } */

void test_2c (int i)
{
  struct s2 arr[5];
  arr[i].m_arr[4] = malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'arr\\\[i\\\].m_arr\\\[4\\\]'" "" { target c } } */
/* { dg-warning "leak of 'arr\\\[i\\\].s2::m_arr\\\[4\\\]'" "" { target c++ } .-1 } */


void test_2d (int i, int j)
{
  struct s2 arr[5];
  arr[i].m_arr[j] = malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'arr\\\[i\\\].m_arr\\\[j\\\]'" "" { target c } } */
/* { dg-warning "leak of 'arr\\\[i\\\].s2::m_arr\\\[j\\\]'" "" { target c++ } .-1 } */


/* Example involving fields.  */

struct s3
{
  struct s3 *m_left;
  struct s3 *m_right;  
};

void test_3 (void)
{
  struct s3 *a = (struct s3 *) malloc (sizeof (struct s3));
  a->m_right = (struct s3 *) malloc (sizeof (struct s3)); /* { dg-warning "dereference of possibly-NULL 'a'" } */
  a->m_right->m_left = (struct s3 *) malloc (sizeof (struct s3)); /* { dg-warning "dereference of possibly-NULL '\\*a.m_right'" "" { target c } } */
  /* { dg-warning "dereference of possibly-NULL '\\*a.s3::m_right'" "" { target c++ } .-1 } */
} /* { dg-warning "leak of 'a'" "leak of a" } */
/* { dg-warning "leak of '<unknown>'" "leak of unknown" { target *-*-* } .-1 } */
/* TODO: rather than '<unknown>', we should print 'a->m_right'
   and 'a->m_right->m_left' (PR analyzer/99771).  */


/* Example involving faking inheritance via casts.  */

struct s4_base
{
  int m_placeholder;
};

struct s4_sub
{
  void *m_buffer;
};

static struct s4_sub *
make_s4_sub (void)
{
  struct s4_sub *sub = (struct s4_sub *) malloc (sizeof (struct s4_sub)); /* { dg-message "allocated here" } */
  if (!sub)
    return NULL;
  sub->m_buffer = malloc (1024); /* { dg-message "allocated here" } */
  return sub;
}

void test_4 (void)
{
  struct s4_base *base = (struct s4_base *)make_s4_sub ();
} /* { dg-warning "leak of 'base'" "leak of base" } */
/* { dg-warning "leak of '<unknown>'" "leak of sub buffer" { target *-*-* } .-1 } */
/* TODO: rather than 'unknown', we should print something
   like '((struct s4_sub *)base)->m_buffer' (PR analyzer/99771).  */
