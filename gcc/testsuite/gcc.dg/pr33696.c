/* { dg-do compile } */

/* This used to ICE with type-checking enabled.  */

typedef unsigned char uint8_t;
typedef unsigned int uint_least32_t;
extern int foo (long int __off);
void write (uint_least32_t chunk_len)
{
     uint8_t tmp[4];
     foo (-(long)chunk_len - sizeof(tmp));
}

