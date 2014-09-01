struct _obstack_chunk
{
  char  *limit;
  struct _obstack_chunk *prev;
  char	contents[4];
};

struct obstack
{
  long	chunk_size;
  struct _obstack_chunk* chunk;
  char	*object_base;
  char	*next_free;
  char	*chunk_limit;
  int	temp;
  int   alignment_mask;
  struct _obstack_chunk *(*chunkfun) ();
  void (*freefun) ();
};

struct fooalign {char x; double d;};
union fooround {long x; double d;};

void
_obstack_begin (h, size, alignment, chunkfun, freefun)
     struct obstack *h;
     int size;
     int alignment;
     void *  (*chunkfun) ();
     void (*freefun) ();
{
  register struct _obstack_chunk* chunk;

  if (alignment == 0)
    alignment = ((char *)&((struct fooalign *) 0)->d - (char *)0);
  if (size == 0)
    {
      int extra = 4;
      if (extra < (sizeof (union fooround)))
	extra = (sizeof (union fooround));
    }
}
