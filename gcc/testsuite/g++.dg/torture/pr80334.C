// This used to be dg-do run testcase, but it is invalid at runtime:
// trying to do a placement new of A which is 16-byte sized and aligned
// into a 16-byte buffer at offset 17 bytes from 16-byte aligned address
// is UB.
// { dg-do compile }

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
