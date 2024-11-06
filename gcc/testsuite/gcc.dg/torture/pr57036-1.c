/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

extern void g (void);
int _setjmp();
int read_buf_open (void);
extern __inline __attribute__ ((__always_inline__,__leaf__))
void
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
