/* { dg-do compile } */

extern void g (void);
extern __inline __attribute__ ((__always_inline__,__leaf__))
f ()
{
  g ();
}
struct __jmp_buf_tag *b;
int jpgDecode_convert (unsigned i)
{
  if (i != 0)
    f ();
  read_buf_open ();
  return _setjmp (b);
}
