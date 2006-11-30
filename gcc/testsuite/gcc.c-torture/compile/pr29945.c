/* This test used to ICE on the SPU target. */
extern const char *__ctype_ptr;

parse_real (unsigned char c)
{
  if ((__ctype_ptr[c]&04) && c != '.')
   unget_char ( c);
}
