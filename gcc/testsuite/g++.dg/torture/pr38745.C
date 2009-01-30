/* { dg-do compile } */

union u_u16
{
  unsigned short v;
  struct
    {
      unsigned char lo8, hi8;
    } __attribute__ ((__may_alias__)) u;
} __attribute__ ((__may_alias__));
union u_u32
{
  unsigned int v;
  struct
    {
      u_u16 lo16, hi16;
    } u;
} __attribute__ ((__may_alias__));
union u_u64
{
  struct
    {
      u_u32 lo32, hi32;
    } u;
};
struct Record
{
};
long long
UnpackFullKey (Record & rec, const char *&p)
{
  long long c64 = 0;
  (*(u_u16 *) & (*(u_u32 *) & ( *(u_u64*)&c64).u.lo32.v).u.lo16.v).u.hi8 = 1;
  return c64;
}

