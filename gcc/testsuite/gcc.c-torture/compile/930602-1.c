typedef struct {
 int f[8];
} T;

f (w, l, r)
     T *w;
     unsigned short l, r;
{
  int i;

  for (i = l; i < r; i++)
    g (w->f[i]);
}
