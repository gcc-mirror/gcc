/* PR118442 */
/* { dg-do compile { target { struct_musttail && { external_musttail && { c || c++11 } } } } } */
/* { dg-options "-fprofile-generate -O2" } */
/* { dg-require-profiling "-fprofile-generate" } */

struct Span {
  int test[5];
};

extern void resolveToBufferSlow (struct Span *buffer);

void
resolveToBuffer (struct Span *buffer)
{
  buffer->test[0] = 4;
  [[clang::musttail]] return resolveToBufferSlow (buffer);
}
