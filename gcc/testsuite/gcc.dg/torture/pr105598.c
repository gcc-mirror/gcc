/* { dg-do run } */

typedef struct { unsigned int num; } info_t;
typedef struct { unsigned int flag, type; } block_t;
info_t info;
block_t blocks[] = { {2,0}, {3,0}, {1,0}, {1,0} };

static block_t *
f (info_t *i, block_t *b)
{
  while (1) {
    unsigned int is_last = b->flag & 0x01;
    i->num++;
    if (b->flag & 0x02) {
      if (b->type != 0x1) b->type = b->type;
      b = f (i, b+1);
    }
    if (is_last)
      break;
    b++;
  }
  return b;
}

int
main ()
{
  f(&info, &blocks[0]);
  if (info.num != 4)
    __builtin_abort ();
  return 0;
}
