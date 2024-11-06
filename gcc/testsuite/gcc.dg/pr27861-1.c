/* PR target/27861 */
/* The following code used to cause an ICE during RTL expansion, as
   expand shift was stripping the SUBREG of a rotate shift count, and
   later producing a VAR_DECL tree whose DECL_RTL's mode didn't match
   the VAR_DECL's type's mode.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2" } */

typedef struct sim_state *SIM_DESC;
typedef enum
{
  SIM_OPEN_STANDALONE, SIM_OPEN_DEBUG
}
SIM_RC;
typedef unsigned int unsigned32 __attribute__ ((__mode__ (__SI__)));
typedef unsigned int unsigned64 __attribute__ ((__mode__ (__DI__)));
typedef unsigned32 unsigned_address;
typedef unsigned_address address_word;
static __inline__ unsigned64
  __attribute__ ((__unused__)) ROTR64 (unsigned64 val, int shift)
{
  unsigned64 result;
  result = (((val) >> (shift)) | ((val) << ((64) - (shift))));
  return result;
}
typedef struct _sim_cpu sim_cpu;
enum
{
    TRACE_MEMORY_IDX, TRACE_MODEL_IDX, TRACE_ALU_IDX, TRACE_CORE_IDX,
};
typedef struct _trace_data
{
  char trace_flags[32];
}
TRACE_DATA;
typedef enum
{
    nr_watchpoint_types,
}
watchpoint_type;
typedef struct _sim_watchpoints
{
  TRACE_DATA trace_data;
}
sim_cpu_base;
struct _sim_cpu
{
  sim_cpu_base base;
};
struct sim_state
{
  sim_cpu cpu[1];
};
typedef address_word instruction_address;
void trace_result_word1 ();
int
do_dror (SIM_DESC sd, instruction_address cia, int MY_INDEX, unsigned64 x,
	 unsigned64 y)
{
  unsigned64 result;
  result = ROTR64 (x, y);
    {
      if ((((-1) & (1 << (TRACE_ALU_IDX))) != 0
	   && (((&(((&(sd)->cpu[0])))->base.trace_data))->
	       trace_flags)[TRACE_ALU_IDX] != 0))
	trace_result_word1 (sd, ((&(sd)->cpu[0])), TRACE_ALU_IDX, (result));
    }
}

