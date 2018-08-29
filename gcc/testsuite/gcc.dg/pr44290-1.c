/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static void __attribute__((naked))
foo(void *from, void *to)
{
  asm volatile("dummy"::"r"(from), "r"(to));
}

unsigned int fie[2];

void fum(void *to)
{
  foo(fie, to);
}

/* { dg-final { scan-tree-dump "foo \\\(void \\\* from, void \\\* to\\\)" "optimized" } } */
