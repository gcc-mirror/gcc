// { dg-do run }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR128: Casting between enum types

extern "C" void abort(void);

enum E1 { BLACK = 0, RED = 1 };
enum E2 { WHITE = 0, YELLOW = 1};

int main(void)
{
  E1 e1 = RED;
  E2 e2 = static_cast<E2>(e1);
  if (e2 != YELLOW)
    abort();
  return 0;
}
