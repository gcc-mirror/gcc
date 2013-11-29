/* { dg-do compile } */

typedef enum
{
  XYZZY,
} enumType;

typedef struct
{
  unsigned char More : 1;
} tResp;

typedef struct
{
  enumType QueryType;
  union
    {
      tResp l[0];
    } u;
} tQResp;

void test(void)
{
  tQResp *qResp = (0);
  if (qResp->u.l[0].More == 0)
    return;
}
