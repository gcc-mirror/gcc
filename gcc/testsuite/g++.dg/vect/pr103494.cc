/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

void glFinish();
struct _Vector_base {
  struct {
    unsigned _M_start;
  } _M_impl;
};
class vector : _Vector_base {
public:
  vector(long) {}
  unsigned *data() { return &_M_impl._M_start; }
};
void *PutBitsIndexedImpl_color_table;
int PutBitsIndexedImpl_dstRectHeight;
char *PutBitsIndexedImpl_src_ptr;
void PutBitsIndexedImpl() {
  vector unpacked_buf(PutBitsIndexedImpl_dstRectHeight);
  unsigned *dst_ptr = unpacked_buf.data();
  for (int x; x; x++) {
    char i = *PutBitsIndexedImpl_src_ptr++;
    dst_ptr[x] = static_cast<int *>(PutBitsIndexedImpl_color_table)[i];
  }
  glFinish();
}
