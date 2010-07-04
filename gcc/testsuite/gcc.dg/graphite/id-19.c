void byte_insert_op1 (unsigned char *loc, unsigned char *end)
{
  register unsigned char *pto = end + 1 + 2;
  while (end != loc)
    *--pto = *--end;
}
