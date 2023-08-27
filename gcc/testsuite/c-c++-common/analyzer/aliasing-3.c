#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct s1
{
  int f1;
};

static struct s1 *p1_glob = NULL;

void test_1 (struct s1 **pp1, struct s1 *p1_parm)
{
  struct s1 *init_p1_glob = p1_glob;

  __analyzer_eval (p1_glob == init_p1_glob); /* { dg-warning "TRUE" } */

  if (!p1_glob)
    return;

  __analyzer_eval (p1_glob == init_p1_glob); /* { dg-warning "TRUE" } */
  __analyzer_eval (p1_glob != NULL); /* { dg-warning "TRUE" } */

  *pp1 = p1_parm;

  /* The write through *pp1 can't have changed p1_glob, because
     we never take a pointer to p1_glob (and it's static to this TU).  */
  __analyzer_eval (p1_glob == init_p1_glob); /* { dg-warning "TRUE" } */
  __analyzer_eval (p1_glob != NULL); /* { dg-warning "TRUE" } */
}

struct s2
{
  int f1;
};

static struct s2 *p2_glob = NULL;

void test_2 (struct s2 **pp2, struct s2 *p2_parm)
{
  /* Ensure that p2_glob is modified.  */
  p2_glob = (struct s2 *) __builtin_malloc (sizeof (struct s2));
  if (!p2_glob)
    return;

  __analyzer_eval (p2_glob != NULL); /* { dg-warning "TRUE" } */

  *pp2 = p2_parm;

  /* The write through *pp2 can't have changed p2_glob, because
     we never take a pointer to p2_glob (and it's static to this TU).  */
  __analyzer_eval (p2_glob != NULL); /* { dg-warning "TRUE" } */
}

struct s3
{
  int f1;
};

struct s3 *p3_glob = NULL;

void test_3 (struct s3 **pp3, struct s3 *p3_parm)
{
  p3_glob = (struct s3 *) __builtin_malloc (sizeof (struct s3));
  if (!p3_glob)
    return;

  __analyzer_eval (p3_glob != NULL); /* { dg-warning "TRUE" } */

  *pp3 = p3_parm;

  /* The write through *pp3 could have changed p3_glob, because
     another TU could take a pointer to p3_glob.  */
  __analyzer_eval (p3_glob != NULL); /* { dg-warning "UNKNOWN" } */
}
