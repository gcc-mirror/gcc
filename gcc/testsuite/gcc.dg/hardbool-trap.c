/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */

typedef char __attribute__ ((__hardbool__ (1))) hbool;

hbool var;

int main () {
  __builtin_memset (&var, 0, sizeof (var));
  (void)var;
}

/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "optimized" } } */
