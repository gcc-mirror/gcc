typedef struct { long addr; long type; } object;

object bar (object blah)
{
  abort();
}

object foo (object x, object y)
{
  object z = *(object*)(x.addr);
  if (z.type & 64)
    {
      y = *(object*)(z.addr+sizeof(object));
      z = *(object*)(z.addr);
      if (z.type & 64)
        y = bar(y);
    }
  return y;
}

int nil;
object cons1[2] = { {(long) &nil, 0}, {(long) &nil, 0} };
object cons2[2] = { {(long) &cons1, 64}, {(long) &nil, 0} };

main()
{
  object x = {(long) &cons2, 64};
  object y = {(long) &nil, 0};
  object three = foo(x,y);
  return 0;
}
