/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */

struct normal_encoding {};
struct unknown_encoding {};
static const struct normal_encoding latin1_encoding = {};

struct encoding*
XmlInitUnknownEncoding(void *mem)
{
  int i;
  struct unknown_encoding *e = mem;
  for (i = 0; i < sizeof(struct normal_encoding); i++)
    ((char *)mem)[i] = ((char *)&latin1_encoding)[i];
  return 0;
}
