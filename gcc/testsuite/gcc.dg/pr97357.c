/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>
#include <stdlib.h>

void * my_malloc (size_t size);

typedef struct glk {
  struct glk *nxt;
} glk;

typedef struct Lock
{
 glk ByteLock;
} Lock;

static Lock *l, *lk;

void bytelocks(glk *rethead, jmp_buf jb)
{
  glk *cur, *cur_lk;

  if (( setjmp (jb)) == 0)
    for (cur = &l->ByteLock; cur != ((glk *)0) ; cur = (cur)->nxt)
        for (cur_lk = &lk->ByteLock; cur_lk != ((glk *)0); cur_lk = cur_lk->nxt)
          {
            glk *retrng;

            if(!rethead)
              rethead = (glk *) my_malloc (sizeof(glk));
            retrng = (glk *) my_malloc (sizeof(glk));

            retrng->nxt = rethead;
          }

 return;
}
