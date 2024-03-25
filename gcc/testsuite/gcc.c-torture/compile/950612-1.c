/* { dg-additional-options "-std=gnu89" } */


typedef enum
{
  LODI,
  STO,
  ADDI,
  ADD,
  SUBI,
  SUB,
  MULI,
  MUL,
  DIVI,
  DIV,
  INC,
  DEC
} INSN;

f (pc)
     short *pc;
{
  long long stack[16], *sp = &stack[16], acc = 0;

  for (;;)
    {
      switch ((INSN)*pc++)
	{
	case LODI:
	  *--sp = acc;
	  acc = ((long long)*pc++) << 32;
	  break;
	case STO:
	  return (acc >> 32) + (((((unsigned long long) acc) & 0xffffffff)  & (1 << 31)) != 0);
	  break;
	case ADDI:
	  acc += ((long long)*pc++) << 32;
	  break;
	case ADD:
	  acc = *sp++ + acc;
	  break;
	case SUBI:
	  acc -= ((long long)*pc++) << 32;
	  break;
	case SUB:
	  acc = *sp++ - acc;
	  break;
	case MULI:
	  acc *= *pc++;
	  break;
	case MUL:
	  {
	    long long aux;
	    unsigned char minus;

	    minus = 0;
	    aux = *sp++;
	    if (aux < 0)
	      {
		minus = ~minus;
		aux = -aux;
	      }
	    if (acc < 0)
	      {
		minus = ~minus;
		acc = -acc;
	      }
	    acc = ((((((unsigned long long) acc) & 0xffffffff)  * (((unsigned long long) aux) & 0xffffffff)) >> 32)
		   + ((((unsigned long long) acc) >> 32)  * (((unsigned long long) aux) & 0xffffffff)  + (((unsigned long long) acc) & 0xffffffff)  + (((unsigned long long) aux) >> 32))
		   + (((((unsigned long long) acc) >> 32)  * (((unsigned long long) aux) >> 32)) << 32));
	    if (minus)
	      acc = -acc;
	  }
	  break;
	case DIVI:
	  {
	    short aux;

	    aux = *pc++;
	    acc = (acc + aux / 2) / aux;
	  }
	  break;
	case DIV:
	  {
	    long long aux;
	    unsigned char minus;

	    minus = 0;
	    aux = *sp++;
	    if (aux < 0)
	      {
		minus = ~minus;
		aux = -aux;
	      }
	    if (acc < 0)
	      {
		minus = ~minus;
		acc = -acc;
	      }

	    if (((unsigned long long)acc)  == 0)
	      acc = (unsigned long long)-1 / 2;
	    else if ((((unsigned long long) ((unsigned long long)acc)) & 0xffffffff)  == 0)
	      acc = ((unsigned long long)aux)  / (((unsigned long long) ((unsigned long long)acc)) >> 32);
	    else if ((((unsigned long long) ((unsigned long long)acc)) >> 32)  == 0)
	      acc = ((((unsigned long long)aux)  / ((unsigned long long)acc)) << 32)
		+ ((((unsigned long long)aux)  % ((unsigned long long)acc)) << 32) / ((unsigned long long)acc);
	    else
	      {
		unsigned char shift;
		unsigned long hi;

		shift = 32;
		hi = (((unsigned long long) ((unsigned long long)acc)) >> 32);
		do {
		  if (hi & ((unsigned long)1 << (shift - 1)))
		    break;
		} while (--shift != 0);
		printf("shift = %d\n", shift);
		acc = ((((unsigned long long)aux)  / ((unsigned long long)acc)) << 32)
		  + (((((unsigned long long)aux)  % ((unsigned long long)acc)) << (32 - shift)) + ((((unsigned long long)acc)  >> shift) / 2)) / (((unsigned long long)acc)  >> shift);
	      }

	    if (minus)
	      acc = -acc;
	  }
	  break;
	case INC:
	  acc += 1;
	  break;
	case DEC:
	  acc -= 1;
	  break;
	}
      printf("%08lx.%08lx\n", (long)(((unsigned long long) acc) >> 32) , (long)(((unsigned long long) acc) & 0xffffffff));
    }
}
