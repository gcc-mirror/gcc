/* CTF generation of function pointers.

   In this testcase, Type de-duplication of function type is exercised.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"var_assign_func_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct variable;

typedef struct variable *var_assign_func_t (struct variable *);

typedef struct variable {
  var_assign_func_t *assign_func;
} shell_var_t;

shell_var_t a;
