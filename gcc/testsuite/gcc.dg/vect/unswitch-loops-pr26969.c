/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void
ruby_re_compile_fastmap (char *fastmap, int options)
{
  int j;
  for (j = 0; j < (1 << 8); j++)
    {
      if (j != '\n' || (options & 4))
	fastmap[j] = 1;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
