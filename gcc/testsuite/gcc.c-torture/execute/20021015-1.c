/* PR opt/7409.  */

extern void abort (void);

char g_list[] = { '1' };

void g (void *p, char *list, int length, char **elementPtr, char **nextPtr)
{
  if (*nextPtr != g_list)
    abort ();

  **nextPtr = 0;
}

int main (void)
{
  char *list = g_list;
  char *element;
  int i, length = 100;

  for (i = 0; *list != 0; i++) 
    {
      char *prevList = list;
      g (0, list, length, &element, &list);
      length -= (list - prevList);
    }

  return 0;
}

