/* { dg-do compile } */
/* ??? Using "long" isn't quite right; we're testing vectors of pointers here.
   But since no extant target supports sizeof(long) != sizeof(void*)...  */
/* { dg-require-effective-target vect_long } */

char **      _M_allocate();
void
_M_fill_insert(unsigned int __n)
{
   char **__new_start = _M_allocate();
   char *__tmp = 0;
   for (; __n > 0; --__n, ++__new_start)
     *__new_start = __tmp;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
