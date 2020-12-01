/* PR sanitizer/78106 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=hwaddress -fdump-tree-sanopt-details -ffat-lto-objects" } */

int *variable;

void __attribute__((used)) release()
{
  __builtin_free (variable);
}

int main2(int argc)
{
  *variable = 2;

  if (argc <= 5)
    asm volatile ("call release");

  *variable = 2;
  __builtin_abort ();

  return 0;
}

int main(int argc, char **argv)
{
  variable = (int *)__builtin_malloc (sizeof(int));
  return main2(argc);
}

/* { dg-final { scan-tree-dump-not "Optimizing out(\n|\r\n|\r)  HWASAN_CHECK \\(7, variable.*" "sanopt" } } */
