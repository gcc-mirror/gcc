/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Used to crash in VRP.  */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

class FXObject;
class FXStream
{
  public:FXStream (const FXObject *cont = __null);
  FXStream & operator<< (const unsigned char &v);
};

bool fxsaveGIF (FXStream &store)
{
  int bitsperpixel;
  unsigned char c1;
  c1 = 0x80;
  c1 |= (bitsperpixel - 1) << 4;
  store << c1;
}
