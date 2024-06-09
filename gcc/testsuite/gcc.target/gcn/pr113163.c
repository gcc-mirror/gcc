/* { dg-do compile } */
/* { dg-additional-options "-O2 -ftree-vectorize" } */ 

struct _reent { union { struct { char _l64a_buf[8]; } _reent; } _new; };
static const char R64_ARRAY[] = "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
char *
_l64a_r (struct _reent *rptr,
     long value)
{
  char *ptr;
  char *result;
  int i, index;
  unsigned long tmp = (unsigned long)value & 0xffffffff;
  result = 
          ((
          rptr
          )->_new._reent._l64a_buf)
                               ;
  ptr = result;
  for (i = 0; i < 6; ++i)
    {
      if (tmp == 0)
 {
   *ptr = '\0';
   break;
 }
      *ptr++ = R64_ARRAY[index];
      tmp >>= 6;
    }
}
