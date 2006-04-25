struct { unsigned int num; } *numptr;
void notice (int);
void doit (unsigned int *);

void
rewrite_finalize_block (int x)
{
  unsigned int *tmp;
  while (tmp = (numptr ? &numptr->num : 0), (tmp ? *tmp : 0) > 0)
    {
      tmp = (numptr ? &numptr->num : 0);
      (void) (*tmp ? 0 : notice (x));
      doit (tmp);
    }
}
