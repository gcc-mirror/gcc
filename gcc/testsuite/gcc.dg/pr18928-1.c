/* PR rtl-optimization/18928 */
/* { dg-do compile { target i?86-*-linux* } } */
/* { dg-options "-O2 -fPIC" } */

const char *toHex( unsigned short u )
{
  static char hexVal[5];
  int i = 3;
  while ( i >= 0 ) {
    unsigned short hex = (u & 0x000f);
    if ( hex < 0x0a )
      hexVal[i] = '0'+hex;
    else
      hexVal[i] = 'A'+(hex-0x0a);
    i--;
  }
  hexVal[4] = '\0';
  return hexVal;
}

