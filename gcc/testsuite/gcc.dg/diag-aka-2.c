/* Verify that the "aka" descriptions for typedefs are correctly
   quoted (PR 62170).  */

typedef struct s1 t1;

int foo(t1 *); /* { dg-message "expected 't1 \\*' {aka 'struct s1 \\*'} but argument is of type 't2 \\*' {aka 'struct s2 \\*'}" } */

int bar() {
  typedef struct s2 {int i;} t2;
  t2 pos;
  return foo(&pos); /* { dg-error "incompatible pointer type" } */
}
