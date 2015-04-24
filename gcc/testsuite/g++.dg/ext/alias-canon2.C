// { dg-do compile }
// PR c++/37553
typedef unsigned int ui32;
__extension__ typedef unsigned long long int ui64;

typedef ui32 __attribute__ ((__may_alias__)) ui32a;
typedef ui64 __attribute__ ((__may_alias__)) ui64a;

union u_u32
{
  ui32a v;
} __attribute__ ((__may_alias__));

union u_u64
{
  ui64a v;
  struct
  {
    union u_u32 lo32, hi32;
  } u;
} __attribute__ ((__may_alias__));

void
out_long (ui64 longVal)
{
  if ((*(union u_u64 *) &longVal).u.lo32.v < 0x10000000ul)
    {
      if ((ui32) ((*(union u_u64 *) &longVal).u.lo32.v) < 0x4000u)
        {
          /* do something useful */
        }
    }
}
