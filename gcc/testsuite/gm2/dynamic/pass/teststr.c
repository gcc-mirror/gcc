extern void StrLib_StrCopy(char *b, unsigned int length, char *a, unsigned int);

static char a[50];

void foo (char *b, unsigned int length)
{
  char Copy[length+1];

  strcpy(Copy, b);
  StrLib_StrCopy(b, length, a, 50);
}

