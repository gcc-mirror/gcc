// { dg-do run }

struct A { alignas(16) char c; };
struct B { A unpacked; char d; } __attribute__((packed));

char x;

int
main()
{
  alignas(__BIGGEST_ALIGNMENT__) B b[3];
  for (int i = 0; i < 3; i++) b[i].unpacked.c = 'a' + i;
  for (int i = 0; i < 3; i++)
    {
      auto a = new A(b[i].unpacked);
      x = a->c;
    }
}
