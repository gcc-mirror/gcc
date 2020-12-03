__attribute__ ((noinline))
void
copy (int *a, int *b)
{
  *a=*b;
}
int p, *ptr = &p;
__attribute__ ((noinline))
void
barrier ()
{
  asm ("":"=r"(ptr):"0"(ptr));
}
