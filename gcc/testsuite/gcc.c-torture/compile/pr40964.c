struct alloc2 {
    int bla;
    char * __restrict data;
    char * __restrict data2;
};
struct alloc2 b;
void * f (void)
{
  return b.data;
}
