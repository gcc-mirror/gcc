f (to)
     char *to;
{
  unsigned int wch;
  register length;
  unsigned char tmp;
  unsigned int mult = 10;

  tmp = (wch>>(unsigned int)(length * mult));
  *to++ = (unsigned char)tmp;
}
