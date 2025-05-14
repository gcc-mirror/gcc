/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-funswitch-loops" } */

#include "tree-vect.h"

typedef int type;
typedef type Vec2[2];

struct BytesVec {
    type d[100];
};

__attribute__((noipa)) struct BytesVec
buildVertexBufferData(const Vec2 *origVertices, bool needsZW,
                      unsigned paddingSize, unsigned long t) {
    const unsigned vertexCount = t;
    struct BytesVec data = (struct BytesVec){.d = {0}};
    type *nextVertexPtr = data.d;

    for (unsigned vertexIdx = 0u; vertexIdx < vertexCount; ++vertexIdx) {

        if (vertexIdx > t)
            __builtin_trap();
        __builtin_memcpy(nextVertexPtr, &origVertices[vertexIdx],
                         2 * sizeof(type));
        nextVertexPtr += 2;

        if (needsZW) {
            nextVertexPtr += 2;
        }

        nextVertexPtr += paddingSize;
    }

    return data;
}
Vec2 origVertices[] = {
    {0, 1}, {2, 3}, {4, 5}, {6, 7},
    {8, 9}, {10, 11}, {12, 13}, {14, 15},
    {16, 17}, {18, 19}, {20, 21}, {22, 23},
    {24, 25}, {26, 27}, {27, 28}, {29, 30},
};

int main()
{
  check_vect ();
  struct BytesVec vec
    = buildVertexBufferData(origVertices, false, 0,
			    sizeof(origVertices) / sizeof(origVertices[0]));

  int errors = 0;
  for (unsigned i = 0; i < 100; i++) {
      if (i / 2 < sizeof(origVertices) / sizeof(origVertices[0])) {
	  int ii = i;
	  int e = origVertices[ii / 2][ii % 2];
	  if (vec.d[i] != e)
	    errors++;
      } else {
	  if (vec.d[i] != 0)
	    errors++;
      }
  }
  if (errors)
    __builtin_abort();
  return 0;
}
