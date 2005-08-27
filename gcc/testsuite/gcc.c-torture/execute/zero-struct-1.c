struct g{};
char y[3];
char *f = &y[0];
char *ff = &y[0];
void h(void)
{
  struct g t;
  *((struct g*)(f++)) = *((struct g*)(ff++));
  *((struct g*)(f++)) = (struct g){};
  t = *((struct g*)(ff++));
}

void abort (void);

int main(void)
{
  h();
  if (f != &y[2])
    abort();
  if (ff != &y[2])
    abort();
  return 0;
}
