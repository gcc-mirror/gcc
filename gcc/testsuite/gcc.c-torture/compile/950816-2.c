f ()
{
  int i;
  float a,b,c;
  unsigned char val[2];
  i = func (&c);
  val[0] = c < a ? a : c >= 1.0 ? b : c;
}
