// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

void bar(void) {}

void foo(void)
{
  typedef enum { ABC } DEF;
  bar();
}

int main () {}
