// { dg-do run }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR20: Some clarifications needed for 12.8 para 15 

extern "C" void printf(const char*, ...);
extern "C" void abort(void);

int count = 0;

class Thing {
public:
  Thing() {
  }
  ~Thing() {
  }
  Thing(const Thing&)
  {
    count += 1;
  }
};

Thing f() {
  Thing t;
  return t;
}

int main(void)
{
  Thing t2 = f();
  printf("%d %x\n", count, &t2);
  if (count != 0)
    abort();
  return 0;
}
