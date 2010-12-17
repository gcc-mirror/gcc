typedef struct TypHeader {
  struct TypHeader ** ptr;
} *TypHandle;
void PlainRange (TypHandle hdList, long lenList, long low, long inc)
{
  long i;
  for (i = 1; i <= lenList; i++ )
    (((TypHandle*)((hdList)->ptr))[i] = (((TypHandle) (((long)(low + (i-1) *
inc) << 2) + 1))));
}
