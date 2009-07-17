struct X { int flag; int pos; };
int foo(struct X *a, struct X *b)
{
  while (1)
    {
      if (a->flag)
	break;
      ({ struct X *tmp = a; a = b; b = tmp; });
    }

  return a->pos + b->pos;
}
