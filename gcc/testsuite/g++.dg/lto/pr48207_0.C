// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

void bar(int) {}

void foo(void)
{
  typedef enum { ABC } DEF;
  DEF a;
  bar((int)a);
}

int main() {}
