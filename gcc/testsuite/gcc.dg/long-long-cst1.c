/* PR middle-end/27724 */
/* { dg-do run } */
/* { dg-options "" } */

extern void abort();

struct st{
  int _mark;
};
unsigned long long t = ((int)(__UINTPTR_TYPE__)&(((struct st*)16)->_mark) - 32);

int main()
{
  if (t != (unsigned long long)(int)-16)
    abort ();
  return 0;
}

