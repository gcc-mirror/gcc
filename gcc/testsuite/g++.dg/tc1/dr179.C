// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR179: Function pointers and subtraction

void foo(void);
typedef void (*fp)(void);

int main()
{
  fp f1 = foo;
  fp f2 = foo;
  (void)f2-f1;  // { dg-error "" "cannot subtract pointers to function" }
}
