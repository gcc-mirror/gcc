/* PR/11640 */

int
internal_insn_latency (int insn_code, int insn2_code)
{
  switch (insn_code)
    {
    case 256:
      switch (insn2_code)
	{
	case 267:
	  return 8;
	case 266:
	  return 8;
	case 265:
	  return 8;
	case 264:
	  return 8;
	case 263:
	  return 8;
	}
      break;
    case 273:
      switch (insn2_code)
	{
	case 267:
	  return 5;
	case 266:
	  return 5;
	case 277:
	  return 3;
	}
      break;
    }
  return 0;
}
