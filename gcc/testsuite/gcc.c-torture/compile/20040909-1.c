static __inline__ int
one_utf8_to_utf16 () { }

static __inline__ unsigned char
conversion_loop (int (*const one_conversion)())
{
return one_conversion ();
}
static unsigned char
convert_utf8_utf16 ()
{
  return conversion_loop (one_utf8_to_utf16);
}
