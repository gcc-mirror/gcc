
typedef typeof(sizeof(char)) Size_t;

#define bufsize ((1L << (8 * sizeof(Size_t) - 2))-256)

struct huge_struct
{
  short buf[bufsize];
  int a;
  int b;
  int c;
  int d;
};

union huge_union
{
  int a;
  char buf[bufsize];
};

unsigned long union_size()
{
  return sizeof(union huge_union);
}

unsigned long struct_size()
{
  return sizeof(struct huge_struct);
}

unsigned long struct_a_offset()
{
  return (unsigned long)(&((struct huge_struct *) 0)->a);
}

int main()
{
  /* Check the exact sizeof value. bufsize is aligned on 256b. */
  if (union_size() != sizeof(char) * bufsize)
    abort();

  if (struct_size() != sizeof(short) * bufsize + 4*sizeof(int))
    abort();

  if (struct_a_offset() < sizeof(short) * bufsize)
    abort();  

  return 0;
}

