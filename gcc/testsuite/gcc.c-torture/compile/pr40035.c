typedef __SIZE_TYPE__ size_t;
void *memmove (void *dest, const void *src, size_t count);
size_t strlen (const char *s);

int
foo (char *param, char *val)
{
  if (val)
    {
      if (val == param + strlen (param) + 1)
        val[-1] = '=';
      else if (val == param + strlen (param) + 2)
        {
          val[-2] = '=';
          memmove (val - 1, val, strlen (val) + 1);
          val--;
        }
    }
  return 0;
}
