char eparr[1];

void *
warn_realloc_extern_ptrarr_offset (int i, int n)
{
  return __builtin_realloc (eparr + i, n); /* { dg-warning "'__builtin_realloc' called on unallocated object 'eparr'" } */
}
