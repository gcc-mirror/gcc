/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

typedef int int32_t;
struct UNewTrie
{
  int32_t index[(0x110000 >> 1)];
};
typedef struct UNewTrie UNewTrie;
utrie_open_3_4 ()
{
  UNewTrie *trie;
  int32_t i, j;
    {
      i = 0;
      do
        {
          trie->index[i++] = j;
          j += 1;
        }
      while (i < 5);
    }
}
/* { dg-final { cleanup-tree-dump "vect" } } */
