typedef void ft(int);
const ft f;
void f2(ft *p __attribute__((const)))
{
  p = f;
}
