/* PR target/11475 */
/* Origin: <heinrich.brand@fujitsu-siemens.com> */

/* This used to fail on SPARC because of a broken pattern.  */

#pragma pack(2)

struct
{
  unsigned char G936:7;
  unsigned short G937:6;
  unsigned int :4;
  unsigned short :14;
  unsigned int G938:8;
#if __INT_MAX__ >= 2147483647L
  unsigned int :30;
#endif
  unsigned short :16;
#if __INT_MAX__ >= 2147483647L
  unsigned int :18;
#endif
  unsigned short G939:9;
} G928b;

void TestG928(void)
{
  G928b.G936 |= G928b.G939;
}
