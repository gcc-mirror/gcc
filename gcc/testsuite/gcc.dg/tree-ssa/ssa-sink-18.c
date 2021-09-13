/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink-stats" } */

#include <stdint.h>

#define HLOG 16
#define        MAX_LIT        (1 <<  5)
typedef const uint8_t *LZF_HSLOT;
typedef LZF_HSLOT LZF_STATE[1 << (HLOG)];

int
compute_on_bytes (uint8_t *in_data, int in_len, uint8_t *out_data, int out_len)
{
  LZF_STATE htab;

  uint8_t *ip = in_data;
  uint8_t *op = out_data;
  uint8_t *in_end = ip + in_len;
  uint8_t *out_end = op + out_len;
  uint8_t *ref;

  unsigned long off;
  unsigned int hval;
  int lit;

  if (!in_len || !out_len)
    return 0;

  lit = 0;
  op++;
  hval = (((ip[0]) << 8) | ip[1]);

  while (ip < in_end - 2)
    {
      uint8_t *hslot;

      hval = (((hval) << 8) | ip[2]);
      hslot = (uint8_t*)(htab + (((hval >> (3 * 8 - 16)) - hval * 5) & ((1 << (16)) - 1)));

      ref = *hslot + in_data;
      *hslot = ip - in_data;

      if (1 && (off = ip - ref - 1) < (1 << 13) && ref > in_data
	  && ref[2] == ip[2]
	  && ((ref[1] << 8) | ref[0]) == ((ip[1] << 8) | ip[0]))
	{
	  unsigned int len = 2;
	  unsigned int maxlen = in_end - ip - len;
	  maxlen
	    = maxlen > ((1 << 8) + (1 << 3)) ? ((1 << 8) + (1 << 3)) : maxlen;

	  if ((op + 3 + 1 >= out_end) != 0)
	    if (op - !lit + 3 + 1 >= out_end)
	      return 0;

	  op[-lit - 1] = lit - 1;
	  op -= !lit;

	  for (;;)
	    {
	      if (maxlen > 16)
		{
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;

		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;

		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;

		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		  len++;
		  if (ref[len] != ip[len])
		    break;
		}

	      do
		{
		  len++;
		}
	      while (len < maxlen && ip[len] == ref[len]);

	      break;
	    }

	  len -= 2;
	  ip++;

	  if (len < 7)
	    {
	      *op++ = (off >> 8) + (len << 5);
	    }
	  else
	    {
	      *op++ = (off >> 8) + (7 << 5);
	      *op++ = len - 7;
	    }
	  *op++ = off;
	  lit = 0;
	  op++;
	  ip += len + 1;

	  if (ip >= in_end - 2)
	    break;

	  --ip;
	  --ip;

	  hval = (((ip[0]) << 8) | ip[1]);
	  hval = (((hval) << 8) | ip[2]);
	  htab[(((hval >> (3 * 8 - 16)) - hval * 5) & ((1 << (16)) - 1))]
	    = (LZF_HSLOT)(ip - in_data);
	  ip++;

	  hval = (((hval) << 8) | ip[2]);
	  htab[(((hval >> (3 * 8 - 16)) - hval * 5) & ((1 << (16)) - 1))]
	    = (LZF_HSLOT)(ip - in_data);
	  ip++;
	}
      else
	{
	  if (op >= out_end)
	    return 0;

	  lit++;
	  *op++ = *ip++;

	  if (lit == (1 << 5))
	    {
	      op[-lit - 1] = lit - 1;
	      lit = 0;
	      op++;
	    }
	}
    }
  if (op + 3 > out_end) /* at most 3 bytes can be missing here */
    return 0;

  while (ip < in_end)
    {
      lit++;
      *op++ = *ip++;
      if (lit == MAX_LIT)
	{
	  op[-lit - 1] = lit - 1; /* stop run */
	  lit = 0;
	  op++; /* start run */
	}
    }

  op[-lit - 1] = lit - 1; /* end run */
  op -= !lit;		  /* undo run if length is zero */

  return op - out_data;
 }

 /* For this case, pass sink2 sinks statements from hot loop header to loop
    exits after gimple loop optimizations, which generates instructions executed
    each iteration in loop, but the results are used outside of loop:
    With -m64,
    "Sinking _367 = (uint8_t *) _320;
    from bb 31 to bb 90
    Sinking _320 = _321 + ivtmp.25_326;
    from bb 31 to bb 90
    Sinking _321 = (unsigned long) ip_229;
    from bb 31 to bb 90
    Sinking len_158 = _322 + 4294967295;
    from bb 31 to bb 33"
    When -m32, Power and X86 will sink 3 instructions, but arm ilp32 couldn't
    sink due to ivopts chooses two IV candidates instead of one, which is
    expected, so this case is restricted to lp64 only so far.  */

 /* { dg-final { scan-tree-dump-times "Sunk statements: 4" 1 "sink2" { target lp64 } } } */
