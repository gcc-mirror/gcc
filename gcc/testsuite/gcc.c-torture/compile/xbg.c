typedef short type;

short
foo (type *sp, int a)
{
  type t;
  int i;

  t = sp[a];
  i = (int)(type)sp[a];
  if (i)
    return 0;
  return t;
}
