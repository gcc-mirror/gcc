/* BTF generation for variables. */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* We expect 6 variables */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 6 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'x1'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_INT" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'bar'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_UNION" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'lala'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_ENUM" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'arr'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_ARRAY" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'plong'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'st_inst'\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(BTF_KIND_STRUCT 'st'" 1 } } */

unsigned int x1;

struct st
{
  int a;
  int b;
};

union {
  long int value;
  struct st * pointer;
} bar;

enum
{
  FOO = 0,
  BAR = 2,
  BAZ,
} lala;

int arr[10][20];

unsigned long * plong;

struct st st_inst;
