// { dg-do compile { target i?86-pc-cygwin } }
// { dg-do compile { target i?86-pc-mingw* } }

// Check that selectany attribute puts symbols into link-once sections.

// { dg-final { scan-assembler "\.section\t\.data\\\$foo\[^\n\]*\n\t\.linkonce discard" } }
// { dg-final { scan-assembler "\.section\t\.data\\\$x\[^\n\]*\n\t\.linkonce discard" } }
// { dg-final { scan-assembler-dem "\nguard variable for x:" } }

__declspec (selectany) int foo = 1;

class X
{
private:
  int m_i;
public:
  X(int i): m_i(i){}
  ~X(){};
};

__declspec(selectany) X x(1);
