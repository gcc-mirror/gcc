/* Verify that the "aka" descriptions for typedefs are correctly
   quoted (PR 62170).  */

/* Exercise %H and %I.  */

typedef struct s1 t1;
typedef struct s2 {int i;} t2;

int foo(t1 *);

void test_1 () {
  t2 pos;

  foo (&pos); // { dg-error "cannot convert 't2\\*' {aka 's2\\*'} to 't1\\*' {aka 's1\\*'}" }
}

/* Exercise %T.  */

typedef struct s3
{  
  void m3 ();
} t3;

void test_2 (const s3 *ptr)
{
  ptr->m3 (); // { dg-error "passing 'const s3' as 'this' argument discards qualifiers" }
}

void test_3 (const t3 *ptr)
{
  ptr->m3 (); // { dg-error "passing 'const t3' {aka 'const s3'} as 'this' argument discards qualifiers" }
}
