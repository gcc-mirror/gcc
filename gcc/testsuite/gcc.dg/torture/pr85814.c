int a;
void b(char *c)
{
  c += 4;
  for (int i = 0; i < 4; i++)
    a = *c++ = 2;
}
