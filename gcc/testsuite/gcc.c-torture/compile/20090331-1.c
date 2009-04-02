struct re_pattern_buffer {
    unsigned char *buffer;
    unsigned long int allocated;
};
void byte_regex_compile (struct re_pattern_buffer *bufp,
			 unsigned char *begalt, unsigned char *b)
{
  unsigned char *pfrom;
  unsigned char *pto;

  while ((unsigned long) (b - bufp->buffer + 3) > bufp->allocated)
    {
      unsigned char *old_buffer = bufp->buffer;
      bufp->allocated <<= 1;
      if (old_buffer != bufp->buffer)
	{
	  int incr = bufp->buffer - old_buffer;
	  b += incr;
	}
    }
  pfrom = b;
  pto = b + 3;
  while (pfrom != begalt)
    *--pto = *--pfrom;
}

