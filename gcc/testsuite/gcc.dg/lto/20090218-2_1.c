typedef struct {
} mem_attrs;
int main(void)
{
  return 0;
}
void *malloc(__SIZE_TYPE__ size);
void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);
static mem_attrs * get_mem_attrs () {
  void **slot;
  *slot = malloc (3);
  memcpy (*slot, 0, 3);
}
void set_mem_attributes () {
  get_mem_attrs ();
}
void set_mem_alias_set () {
  get_mem_attrs ();
}
