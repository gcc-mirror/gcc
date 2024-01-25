// PR c++/113598
// { dg-additional-options -w }

struct Cpu
{
  int op_nop();
};
typedef int(Cpu::*OpCode)();
void f()
{
  new OpCode[256]{&Cpu::op_nop};
}
