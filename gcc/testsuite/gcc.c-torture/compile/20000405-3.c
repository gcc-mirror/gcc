struct foo {
  void *entry[40];
} __attribute__ ((aligned(32)));

int foo (struct foo *ptr, int idx, void *pointer)
{
  ptr->entry[idx] = pointer;
  return 0;
}
