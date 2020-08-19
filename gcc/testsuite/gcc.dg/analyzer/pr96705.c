int __attribute__ ((vector_size (8))) v;
int i;

void
test (void)
{
  v &= 0;
  v *= i;
}
