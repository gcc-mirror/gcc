/* { dg-do compile } */
/* { dg-options "-fsanitize=vla-bound -c -O3 -fdump-tree-optimized -ffat-lto-objects" } */

int x;

static inline
__attribute__((no_sanitize("undefined")))
void do_not_sanitize(void)
{
  x++;
}

void
sanitize_this(void)
{
  x++;
  do_not_sanitize();
}

/* { dg-final { scan-tree-dump-times "Function do_not_sanitize" 1 "optimized" } } */
