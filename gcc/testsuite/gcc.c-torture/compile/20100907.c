struct a {int a,b;};
const static struct a a[1]={{1,2}};
struct a b,c;
t()
{
  int idx = 0;
  b=a[idx];
  c=a[idx];
}
