/* { dg-do compile } */
/* { dg-options " -O2 -fno-shrink-wrap-separate -march=rv32imaf_zca_zcmp -mabi=ilp32f" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Os" "-Og" "-O3" "-Oz" "-flto"} } */

typedef struct MAT_PARAMS_S
{
  int N;
  signed short *A;
  signed short *B;
  signed int *C;
} mat_params;

typedef struct CORE_PORTABLE_S
{
  unsigned char portable_id;
} core_portable;

typedef struct RESULTS_S
{
  /* inputs */
  signed short seed1;	   /* Initializing seed */
  signed short seed2;	   /* Initializing seed */
  signed short seed3;	   /* Initializing seed */
  void *memblock[4];	   /* Pointer to safe memory location */
  unsigned int size;	   /* Size of the data */
  unsigned int iterations; /* Number of iterations to execute */
  unsigned int execs;	   /* Bitmask of operations to execute */
  struct list_head_s *list;
  mat_params mat;
  /* outputs */
  unsigned short crc;
  unsigned short crclist;
  unsigned short crcmatrix;
  unsigned short crcstate;
  signed short err;
  /* ultithread specific */
  core_portable port;
} core_results;

extern signed short
core_bench_state (unsigned int, void *, signed short, signed short,
		  signed short, unsigned short);

extern signed short
core_bench_matrix (mat_params *, signed short, unsigned short);

extern unsigned short
crcu16 (signed short, unsigned short);

signed short
calc_func (signed short *pdata, core_results *res)
{
  signed short data = *pdata;
  signed short retval;
  unsigned char optype
    = (data >> 7)
      & 1;    /* bit 7 indicates if the function result has been cached */
  if (optype) /* if cached, use cache */
    return (data & 0x007f);
  else
    { /* otherwise calculate and cache the result */
      signed short flag
	= data & 0x7; /* bits 0-2 is type of function to perform */
      signed short dtype
	= ((data >> 3) & 0xf); /* bits 3-6 is specific data for the operation */
      dtype |= dtype << 4; /* replicate the lower 4 bits to get an 8b value */
      switch (flag)
	{
	case 0:
	  if (dtype < 0x22) /* set min period for bit corruption */
	    dtype = 0x22;
	  retval = core_bench_state (res->size, res->memblock[3], res->seed1,
				     res->seed2, dtype, res->crc);
	  if (res->crcstate == 0)
	    res->crcstate = retval;
	  break;
	case 1:
	  retval = core_bench_matrix (&(res->mat), dtype, res->crc);
	  if (res->crcmatrix == 0)
	    res->crcmatrix = retval;
	  break;
	default:
	  retval = data;
	  break;
	}
      res->crc = crcu16 (retval, res->crc);
      retval &= 0x007f;
      *pdata = (data & 0xff00) | 0x0080 | retval; /* cache the result */
      return retval;
    }
}

/* { dg-final { scan-assembler "cm\.push" } } */
