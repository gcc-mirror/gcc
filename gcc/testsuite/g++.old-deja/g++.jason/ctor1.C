// PRMS Id: 5584

extern "C"
{
  struct xx {
    void (*xx)(void);
    int x,y;
  };
}

int r = 1;

void f(void)
{
  r = 0;
}

int main()
{
  struct xx p;

  p.xx = f;
  p.xx();

  return r;
}
