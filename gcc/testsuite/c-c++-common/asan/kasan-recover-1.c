/* { dg-do compile } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address --param asan-instrumentation-with-call-threshold=100 -ffat-lto-objects" } */

void
foo (int *p)
{
  *p = 0;
}

/* { dg-final { scan-assembler "__asan_report_store4_noabort" } } */

