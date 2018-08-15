/* Verify the "aka" descriptions for typedefs are correctly
   quoted and shown within labels.  */

/* { dg-options "-fdiagnostics-show-caret" } */

typedef struct s1 t1;
typedef struct s2 {int i;} t2;

int foo(t1 *);

void test_1 () {
  t2 pos;

  foo (&pos); // { dg-error "cannot convert 't2\\*' {aka 's2\\*'} to 't1\\*' {aka 's1\\*'}" }
  /* { dg-begin-multiline-output "" }
   foo (&pos);
        ^~~~
        |
        t2* {aka s2*}
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 int foo(t1 *);
         ^~~~
     { dg-end-multiline-output "" } */
}
