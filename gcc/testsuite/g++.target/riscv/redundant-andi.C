/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcb -mabi=lp64" { target rv64 } } */
/* { dg-options "-O2 -march=rv32gcb -mabi=ilp32" { target rv32 } } */


typedef int move_s;
struct state_t
{
  int npieces[13];
};
typedef struct state_t state_t;
int
search (state_t *s, int alpha, int beta, int depth, int is_null, int cutnode,
	int extend, int wpcs, int bpcs, move_s moves[240])
{
  int i;
  if ((((moves[i]) >> 19) & 0x0F) != 13
      && (((moves[i]) >> 19) & 0x0F) != 1 && (((moves[i]) >> 19) & 0x0F) != 2)
    if ((wpcs + bpcs) == 1)
      extend += 4;
  return extend;
}

/* A splitter was generating an unnecessary andi instruction.  Verify it's
   not in our output.  */
/* { dg-final { scan-assembler-not "andi\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],-1" } } */
