int isdigit (int);
int f (const char *type)
{
  return isdigit ((unsigned char) *type++);
}
