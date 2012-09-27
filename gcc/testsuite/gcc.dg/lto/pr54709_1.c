void * memcpy (void *, void *, long);
void bar (void *p, void *q, unsigned s)
{
  memcpy (p, q, s);
}
