/* SH has special handling for combined and/shift sequences.  Make
   sure that it behaves properly when one input is in the MACL register.  */
int r, t;

static void initRGB()
{
  t = ((r*255/3) & 0xff) << 16;
}
