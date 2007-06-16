quantize_fs_dither (unsigned width, short *errorptr, int dir)
{
  short bpreverr;
  unsigned col;
  for (col = width; col > 0; col--) 
    errorptr += dir;
  errorptr[0] = (short) bpreverr;
}


