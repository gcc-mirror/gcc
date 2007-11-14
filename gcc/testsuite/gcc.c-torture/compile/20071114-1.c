/* PR tree-optimization/34046 */
/* Origin: dcb <dcb314@hotmail.com> */

typedef unsigned char bool8;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;

struct SIAPU
{
    uint8 *PC;
    uint8 *RAM;
    uint8 Bit;
    uint32 Address;
    uint8 *WaitAddress1;
    uint8 *WaitAddress2;
    uint8 _Carry;
};

struct SAPU
{
    bool8 ShowROM;
    uint8 OutPorts [4];
    uint8 ExtraRAM [64];
    uint16 TimerTarget [3];
};

struct SAPU APU;
struct SIAPU IAPU;

void S9xSetAPUControl (uint8 byte);
void S9xSetAPUDSP (uint8 byte);
uint8 S9xGetAPUDSP ();

uint8 S9xAPUGetByte (uint32 Address)
{
  Address &= 0xffff;

  if (Address <= 0xff && Address >= 0xf0)
    {
      if (Address >= 0xf4 && Address <= 0xf7)
	{
	  IAPU.WaitAddress2 = IAPU.WaitAddress1;
	  IAPU.WaitAddress1 = IAPU.PC;
	  return (IAPU.RAM [Address]);
	}
      else if (Address == 0xf3)
	return (S9xGetAPUDSP ());

      if (Address >= 0xfd)
	{
	  IAPU.WaitAddress2 = IAPU.WaitAddress1;
	  IAPU.WaitAddress1 = IAPU.PC;
	  uint8 t = IAPU.RAM [Address];
	  IAPU.RAM [Address] = 0;
	  return (t);
	}

      return (IAPU.RAM [Address]);
    }
 else
   return (IAPU.RAM [Address]);
}

void S9xAPUSetByte (uint8 byte, uint32 Address)
{
  Address &= 0xffff;

  if (Address <= 0xff && Address >= 0xf0)
    {
      if (Address == 0xf3)
	S9xSetAPUDSP (byte);
      else if (Address >= 0xf4 && Address <= 0xf7)
	APU.OutPorts [Address - 0xf4] = byte;
      else if (Address == 0xf1)
	S9xSetAPUControl (byte);
      else if (Address < 0xfd)
	{
	  IAPU.RAM [Address] = byte;
	  if (Address >= 0xfa)
	    {
	      if (byte == 0)
		APU.TimerTarget [Address - 0xfa] = 0x100;
	      else
		APU.TimerTarget [Address - 0xfa] = byte;
	    }
	}
    }
  else
    {
      if (Address < 0xffc0)
	IAPU.RAM [Address] = byte;
      else
	{
	  APU.ExtraRAM [Address - 0xffc0] = byte;
	  if (!APU.ShowROM)
	    IAPU.RAM [Address] = byte;
	}
    }
}

void ApuCA ()
{
  IAPU.Address = *(uint16 *) (IAPU.PC + 1);
  IAPU.Bit = (uint8)(IAPU.Address >> 13);
  if ((IAPU._Carry))
    S9xAPUSetByte (S9xAPUGetByte (IAPU.Address) | (1 << IAPU.Bit), IAPU.Address);
  else
    S9xAPUSetByte (S9xAPUGetByte (IAPU.Address) & ~(1 << IAPU.Bit), IAPU.Address);
}
