/* PR target/98849 */

unsigned int a[1024], b[1024];
int c[1024], d[1024];

void
f1 (void)
{
  for (int i = 0; i < 1024; i++)
    a[i] = b[i] << 3;
}

void
f2 (int x)
{
  for (int i = 0; i < 1024; i++)
    a[i] = b[i] << x;
}

void
f3 (void)
{
  for (int i = 0; i < 1024; i++)
    c[i] = d[i] << 3;
}

void
f4 (int x)
{
  for (int i = 0; i < 1024; i++)
    c[i] = d[i] << x;
}

void
f5 (void)
{
  for (int i = 0; i < 1024; i++)
    a[i] = b[i] >> 3;
}

void
f6 (int x)
{
  for (int i = 0; i < 1024; i++)
    a[i] = b[i] >> x;
}

void
f7 (void)
{
  for (int i = 0; i < 1024; i++)
    c[i] = d[i] >> 3;
}

void
f8 (int x)
{
  for (int i = 0; i < 1024; i++)
    c[i] = d[i] >> x;
}
