extern void free (void *__ptr);
struct shparam
{
  char **p;
  int foo;
};
static struct shparam shellparam;
inline void freeparam (volatile struct shparam *param, char **ap)
{
  free ((void *) (*ap));
  free ((void *) (param->p));
}
void dotcmd (char **p)
{
  freeparam (&shellparam, p);
}
void evaltree (void)
{
  void (*evalfn) (char **);
  evalfn = dotcmd;
}
