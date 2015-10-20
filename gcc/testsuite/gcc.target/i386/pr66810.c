/* { dg-do compile { target ia32 } } */
/* { dg-options "-mno-sse -mno-mmx -miamcu" } */

int vv;

void
i (void)
{
  static int a[vv]; /* { dg-error "storage size" } */
}
