/* { dg-do compile } */

typedef struct {
  void *child[2];
  char otherbits;
} critbit0_node;

int allprefixed_traverse(char *top)
{
  if (top)
    {
      critbit0_node *q = (void *)top - 1;
      int direction = 0;
      for (;; ++direction)
	switch (allprefixed_traverse(q->child[direction]))
	  {
	  case 1:
	    break;
	  case 0:
	    return 0;
	  default:
	    return 1;
	  }
    }
}
