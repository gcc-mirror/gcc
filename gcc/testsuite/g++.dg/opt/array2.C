/* { dg-do compile } */
/* { dg-options "-O2" } */
struct JArray
{
  int data[1];
};
void *copyIntoByteArray (struct JArray *dest, __SIZE_TYPE__ offset)
{
  void *pdest = dest->data + offset;
  return pdest;
}
