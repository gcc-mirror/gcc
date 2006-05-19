// PR c++/27471

struct A { unsigned a:8; };

extern void b(unsigned char);

void breakme (A f)
{
  b((unsigned char) f.a);
}
