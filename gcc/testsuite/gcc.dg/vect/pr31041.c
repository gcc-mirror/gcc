/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

struct UNewTrie
{
  int index[(0x110000 >> 1)];
};
typedef struct UNewTrie UNewTrie;
void
utrie_open_3_4 ()
{
  UNewTrie *trie;
  int i, j;
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
