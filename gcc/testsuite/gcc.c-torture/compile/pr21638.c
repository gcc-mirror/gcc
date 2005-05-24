typedef struct hashhdr {
 int bitmaps[32];
} HASHHDR;

static void
swap_header_copy(HASHHDR *srcp, HASHHDR *destp)
{
  int i;
  for (i = 0; i < 32; i++)
    ((char *)&(destp->bitmaps[i]))[0] = ((char *)&(srcp->bitmaps[i]))[1];
}

int
flush_meta(HASHHDR *whdrp1)
{
 HASHHDR *whdrp;
 HASHHDR whdr;
 whdrp = &whdr;
 swap_header_copy(whdrp1, whdrp);
 return (0);
}
