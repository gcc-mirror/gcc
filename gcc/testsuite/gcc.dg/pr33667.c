/* { dg-do compile } */
/* { dg-options "-O2" } */

__extension__ typedef __SIZE_TYPE__ size_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned long long int uint64_t;
struct magic {
  uint8_t mask_op;
  union {
   uint64_t _mask;
  } _u;
  union VALUETYPE {
   uint16_t h;
  } value;
};
void cvt_16(union VALUETYPE *p, const struct magic *m)
{
  if (m->_u._mask)
    p->h %= (uint16_t) m->_u._mask;
}
