// { dg-do assemble  }
char *t1 (const char *s)
{
  return const_cast<char *>(s);
}

char *&t1 (const char *&s)
{
  return const_cast<char *&>(s);
}
