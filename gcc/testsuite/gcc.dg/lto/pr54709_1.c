void * memcpy (void *, void *, __SIZE_TYPE__);
void bar (void *p, void *q, unsigned s)
{
  memcpy (p, q, s);
}
