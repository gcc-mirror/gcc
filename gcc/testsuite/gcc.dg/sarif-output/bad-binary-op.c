/* { dg-options "-fdiagnostics-show-caret -fdiagnostics-add-output=sarif" } */

struct s {};
struct t {};

typedef struct s S;
typedef struct t T;

extern S callee_4a (void);
extern T callee_4b (void);

int test_4 (void)
{
  return callee_4a () + callee_4b (); /* { dg-error "invalid operands to binary \+" } */

/* { dg-begin-multiline-output "" }
   return callee_4a () + callee_4b ();
          ~~~~~~~~~~~~ ^ ~~~~~~~~~~~~
          |              |
          |              T {aka struct t}
          S {aka struct s}
   { dg-end-multiline-output "" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest bad-binary-op.c "bad-binary-op.py" } } */
