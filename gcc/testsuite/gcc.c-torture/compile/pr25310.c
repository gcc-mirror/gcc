
extern int f (char *, int);

void test (void)
{
  char buffer[65536];
  char *bufptr;
  char *bufend;
  int bytes;

  bufptr = buffer;
  bufend = buffer + sizeof(buffer) - 1;

  while ((bytes = f (bufptr, bufend - bufptr)) > 0)
    bufptr += bytes;
}

