int isascii (int);

int f1 (const char *type)
{
  return isascii ((unsigned char) *type++);
}

