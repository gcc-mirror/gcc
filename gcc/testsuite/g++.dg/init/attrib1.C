// { dg-do run }

void f() __attribute((__constructor__));
int i;
void f() { i = 1; }

int main(int, char **)
{
  return 1-i;
}
