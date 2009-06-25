extern void abort (void);

typedef union i386_operand_type
{
  struct
    {
      unsigned int reg8:1;
      unsigned int reg16:1;
      unsigned int reg32:1;
      unsigned int reg64:1;
      unsigned int floatreg:1;
      unsigned int regmmx:1;
      unsigned int regxmm:1;
      unsigned int regymm:1;
      unsigned int control:1;
      unsigned int debug:1;
      unsigned int test:1;
      unsigned int sreg2:1;
      unsigned int sreg3:1;
      unsigned int imm1:1;
      unsigned int imm8:1;
      unsigned int imm8s:1;
      unsigned int imm16:1;
      unsigned int imm32:1;
      unsigned int imm32s:1;
      unsigned int imm64:1;
      unsigned int disp8:1;
      unsigned int disp16:1;
      unsigned int disp32:1;
      unsigned int disp32s:1;
      unsigned int disp64:1;
      unsigned int acc:1;
      unsigned int floatacc:1;
      unsigned int baseindex:1;
      unsigned int inoutportreg:1;
      unsigned int shiftcount:1;
      unsigned int jumpabsolute:1;
      unsigned int esseg:1;
      unsigned int regmem:1;
      unsigned int mem:1;
      unsigned int byte:1;
      unsigned int word:1;
      unsigned int dword:1;
      unsigned int fword:1;
      unsigned int qword:1;
      unsigned int tbyte:1;
      unsigned int xmmword:1;
      unsigned int ymmword:1;
      unsigned int unspecified:1;
      unsigned int anysize:1;
    } bitfield;
  unsigned int array[2];
} i386_operand_type;

unsigned int x00, x01, y00, y01;

int main (int argc, char *argv[])
{
  i386_operand_type a,b,c,d;

  a.bitfield.reg16 = 1;
  a.bitfield.imm16 = 0;
  a.array[1] = 22;

  b = a;
  x00 = b.array[0];
  x01 = b.array[1];

  c = b;
  y00 = c.array[0];
  y01 = c.array[1];

  d = c;
  if (d.bitfield.reg16 != 1)
    abort();
  if (d.bitfield.imm16 != 0)
    abort();
  if (d.array[1] != 22)
    abort();

  return 0;
}
