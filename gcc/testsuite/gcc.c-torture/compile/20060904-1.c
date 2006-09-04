/* PR rtl-optimization/27616 */
/* Reported by Lee Ji Hwan <moonz@kaist.ac.kr> */
/* Testcase by Andrew Pinski <pinskia@gcc.gnu.org> */

struct chunk_s
{
  unsigned int size;
  int offset_next;
};

typedef struct chunk_s chunk_t;

void foo(chunk_t *first)
{
  chunk_t *cur;
  char *first0;

  do {
    first0 = (char *) first;
    cur = (chunk_t *) (first0 + first->offset_next);
    if ((chunk_t *) (first0 + cur->offset_next) != first)
      return ;

    first->offset_next = 0;

  } while (cur->size != 0);
}
