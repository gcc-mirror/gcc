int bar (int (*fn)(const char *))
{
  return fn ("0");
}
