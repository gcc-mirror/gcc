
typedef struct func
{
  int *a;
}func;
__attribute__((noinline))
void ff(struct func *t)
{
  *(t->a) = 0;
}


typedef struct mapped_iterator {
  func F;
}mapped_iterator;

__attribute__((noinline))
mapped_iterator map_iterator(func F) {
  mapped_iterator t = {F};
  return t;
}

void map_to_vector(func *F) {
  mapped_iterator t = map_iterator(*F);
  ff(&t.F);
}
int main() {
  int resultIsStatic = 1;
  func t ={&resultIsStatic};
  map_to_vector(&t);

  if (resultIsStatic)
    __builtin_trap();
  __builtin_exit(0);
}
