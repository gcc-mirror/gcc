/* The problem on IA-64 is that if-conversion creates a sequence

	 (p17) cmp.geu p6, p7 = r48, r15
	 (p16) cmp.gtu p6, p7 = r48, r15

   where p16 and p17 are complemenary, but the assembler DV validation
   code doesn't recognize that p6 and p7 are complimentary, and so
   we end up warning for a later use

	 (p6) addl r14 = 1, r0
	 (p7) mov r14 = r0

   that appears to be a WAW violation. */

/* { dg-prune-output "Assembler messages" } */
/* { dg-prune-output "violate\[^\n\]*dependency" } */
/* { dg-prune-output "first path encountering" } */
/* { dg-prune-output "location of the conflicting" } */

typedef unsigned long int unsigned_word;
typedef signed long int signed_word;
typedef unsigned_word word;

typedef enum { ADD, ADD_CI, ADD_CO, ADD_CIO, SUB, SUB_CI, SUB_CO,
SUB_CIO, ADC_CI, ADC_CO, ADC_CIO, AND, IOR, XOR, ANDC, IORC, EQV,
NAND, NOR, AND_RC, IOR_RC, XOR_RC, ANDC_RC, IORC_RC, EQV_RC, NAND_RC,
NOR_RC, AND_CC, IOR_CC, XOR_CC, ANDC_CC, IORC_CC, EQV_CC, NAND_CC,
NOR_CC, LSHIFTR, ASHIFTR, SHIFTL, LSHIFTR_CO, ASHIFTR_CO, SHIFTL_CO,
ROTATEL, ROTATEL_CO, ROTATEXL_CIO, ASHIFTR_CON, EXTS1, EXTS2, EXTU1,
EXTU2, CLZ, CTZ, FF1, FF0, ABSVAL, NABSVAL, CMP, CPEQ, CPGE, CPGEU,
CPGT, CPGTU, CPLE, CPLEU, CPLT, CPLTU, CPNEQ, CMPPAR, DOZ, COPY,
EXCHANGE, COMCY, } opcode_t;

typedef struct
{
  opcode_t opcode:8;
  unsigned int s1:8;
  unsigned int s2:8;
  unsigned int d:8;
} insn_t;

enum prune_flags
{
  NO_PRUNE = 0,
  CY_0 = 1,
  CY_1 = 2,
  CY_JUST_SET = 4,
};

int flag_use_carry = 1;

inline
recurse(opcode_t opcode,
 int d,
 int s1,
 int s2,
 word v,
 int cost,
 insn_t *sequence,
 int n_insns,
 word *values,
 int n_values,
 const word goal_value,
 int allowed_cost,
 int cy,
 int prune_flags)
{
  insn_t insn;

  allowed_cost -= cost;

  if (allowed_cost > 0)
    {
      word old_d;

      old_d = values[d];
      values[d] = v;

      insn.opcode = opcode;
      insn.s1 = s1;
      insn.s2 = s2;
      insn.d = d;
      sequence[n_insns] = insn;

      synth(sequence, n_insns + 1, values, n_values,
     goal_value, allowed_cost, cy, prune_flags);

      values[d] = old_d;
    }
  else if (goal_value == v)
    {
      insn.opcode = opcode;
      insn.s1 = s1;
      insn.s2 = s2;
      insn.d = d;
      sequence[n_insns] = insn;
      test_sequence(sequence, n_insns + 1);
    }
}

synth(insn_t *sequence,
      int n_insns,
      word *values,
      int n_values,
      word goal_value,
      int allowed_cost,
      int ci,
      int prune_hint)
{
  int s1, s2;
  word v, r1, r2;
  int co;
  int last_dest;

  if (n_insns > 0)
    last_dest = sequence[n_insns - 1].d;
  else
    last_dest = -1;
  if (ci >= 0 && flag_use_carry)
    {
      for (s1 = n_values - 1; s1 >= 0; s1--)
 {
   r1 = values[s1];
   for (s2 = s1 - 1; s2 >= 0; s2--)
     {
       r2 = values[s2];

       if (allowed_cost <= 1 && (prune_hint & CY_JUST_SET) == 0)
  {
    if (last_dest >= 0 && s1 != last_dest && s2 != last_dest)
      continue;
  }
       do { word __d = ( r1) + ( r2) + (( ci)); ( co) = ( ci) ? __d <= ( r1) : __d < ( r1); (v) = __d; } while (0);
       recurse(ADD_CIO, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
       do { word __d = ( r1) + ( r2) + (( ci)); ( co) = ( ci); (v) = __d; } while (0);
       recurse(ADD_CI, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

       do { word __d = ( r1) - ( r2) - (( ci)); ( co) = ( ci) ? __d >= ( r1) : __d > ( r1); (v) = __d; } while (0);
       recurse(SUB_CIO, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
       do { word __d = ( r2) - ( r1) - (( ci)); ( co) = ( ci) ? __d >= ( r2) : __d > ( r2); (v) = __d; } while (0);
       recurse(SUB_CIO, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

       do { word __d = ( r1) - ( r2) - (( ci)); ( co) = ( ci); (v) = __d; } while (0);
       recurse(SUB_CI, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
       do { word __d = ( r2) - ( r1) - (( ci)); ( co) = ( ci); (v) = __d; } while (0);
       recurse(SUB_CI, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

     }
 }
    }
  for (s1 = n_values - 1; s1 >= 0; s1--)
    {
      r1 = values[s1];
      for (s2 = s1 - 1; s2 >= 0; s2--)
 {
   r2 = values[s2];

   if (allowed_cost <= 1)
     {
       if (last_dest >= 0 && s1 != last_dest && s2 != last_dest)
  continue;
     }

   do { word __d = ( r1) + ( r2); ( co) = __d < ( r1); (v) = __d; } while (0);
   recurse(ADD_CO, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

   ((v) = ( r1) + ( r2), ( co) = ( ci));
   recurse(ADD, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   do { word __d = ( r1) - ( r2); ( co) = __d > ( r1); (v) = __d; } while (0);
   recurse(SUB_CO, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
   do { word __d = ( r2) - ( r1); ( co) = __d > ( r2); (v) = __d; } while (0);
   recurse(SUB_CO, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
   ((v) = ( r1) - ( r2), ( co) = ( ci));
   recurse(SUB, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
   ((v) = ( r2) - ( r1), ( co) = ( ci));
   recurse(SUB, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   ((v) = ( r1) & ( r2), ( co) = ( ci));
   recurse(AND, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   ((v) = ( r1) | ( r2), ( co) = ( ci));
   recurse(IOR, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   ((v) = ( r1) ^ ( r2), ( co) = ( ci));
   recurse(XOR, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   ((v) = ( r1) & ~( r2), ( co) = ( ci));
   recurse(ANDC, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
   ((v) = ( r2) & ~( r1), ( co) = ( ci));
   recurse(ANDC, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
   ((v) = ( r1) | ~( r2), ( co) = ( ci));
   recurse(IORC, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
   ((v) = ( r2) | ~( r1), ( co) = ( ci));
   recurse(IORC, n_values,  s2,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
   ((v) = ( r1) ^ ~( r2), ( co) = ( ci));
   recurse(EQV, n_values,  s1,  s2, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

 }
    }
  if (ci >= 0 && flag_use_carry)
    {
      for (s1 = n_values - 1; s1 >= 0; s1--)
 {
   r1 = values[s1];

   if (allowed_cost <= 1 && (prune_hint & CY_JUST_SET) == 0)
     {

       if (last_dest >= 0 && s1 != last_dest)
  continue;
     }

   do { word __d = ( r1) + ( r1) + (( ci)); ( co) = ( ci) ? __d <= ( r1) : __d < ( r1); (v) = __d; } while (0);
   recurse(ADD_CIO, n_values,  s1,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

   do { word __d = ( r1) + ( r1) + (( ci)); ( co) = ( ci); (v) = __d; } while (0);
   recurse(ADD_CI, n_values,  s1,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

   do { word __d = ( r1) + ( -1 ) + (( ci)); ( co) = ( ci) ? __d <= ( r1) : __d < ( r1); (v) = __d; } while (0);
   recurse(ADD_CIO, n_values,  s1,  (0x20 + -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

   do { word __d = ( r1) + ( 0 ) + (( ci)); ( co) = ( ci) ? __d <= ( r1) : __d < ( r1); (v) = __d; } while (0);
   recurse(ADD_CIO, n_values,  s1,  (0x20 + 0) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
   do { word __d = ( 0 ) - ( r1) - (( ci)); ( co) = ( ci) ? __d >= ( 0 ) : __d > ( 0 ); (v) = __d; } while (0);
   recurse(SUB_CIO, n_values,  (0x20 + 0) ,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

 }
    }
  for (s1 = n_values - 1; s1 >= 0; s1--)
    {
      r1 = values[s1];

      if (allowed_cost <= 1)
 {
   if (last_dest >= 0 && s1 != last_dest)
     continue;
 }
      do { word __d = ( r1) + ( r1); ( co) = __d < ( r1); (v) = __d; } while (0);
      recurse(ADD_CO, n_values,  s1,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);

      ((v) = ( r1) & ( 1 ), ( co) = ( ci));
      recurse(AND, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

      ((v) = ( r1) ^ ( 1 ), ( co) = ( ci));
      recurse(XOR, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

      ((v) = ( -1 ) - ( r1), ( co) = ( ci));
      recurse(SUB, n_values,  (0x20 + -1) ,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      do { word __d = ( r1) + ( 1 ); ( co) = __d < ( r1); (v) = __d; } while (0);
      recurse(ADD_CO, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
      ((v) = ( r1) + ( 1 ), ( co) = ( ci));
      recurse(ADD, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      do { word __d = ( r1) + ( -1 ); ( co) = __d < ( r1); (v) = __d; } while (0);
      recurse(ADD_CO, n_values,  s1,  (0x20 + -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
      do { word __d = ( r1) - ( 1 ); ( co) = __d > ( r1); (v) = __d; } while (0);
      recurse(SUB_CO, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
      do { word __d = ( 0 ) - ( r1); ( co) = __d > ( 0 ); (v) = __d; } while (0);
      recurse(SUB_CO, n_values,  (0x20 + 0) ,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET);
      ((v) = ( 0 ) - ( r1), ( co) = ( ci));
      recurse(SUB, n_values,  (0x20 + 0) ,  s1, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      ((v) = ((unsigned_word) ( r1) >> (( 1 ) & (32  - 1)) ), ( co) = ( ci));
      recurse(LSHIFTR, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      ((v) = ((signed_word) ( r1) >> (( 1 ) & (32  - 1)) ), ( co) = ( ci));
      recurse(ASHIFTR, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      ((v) = ((signed_word) ( r1) << (( 1 ) & (32  - 1)) ), ( co) = ( ci));
      recurse(SHIFTL, n_values,  s1,  (0x20 + 1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      ((v) = ((unsigned_word) ( r1) >> (( 32 -1 ) & (32  - 1)) ), ( co) = ( ci));
      recurse(LSHIFTR, n_values,  s1,  (0x20 + 32 -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      ((v) = ((signed_word) ( r1) >> (( 32 -1 ) & (32  - 1)) ), ( co) = ( ci));
      recurse(ASHIFTR, n_values,  s1,  (0x20 + 32 -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
    }
  if (ci >= 0 && flag_use_carry
      && (allowed_cost <= 1 ? ((prune_hint & CY_JUST_SET) != 0) : 1))
    {
      do { word __d = ( 0 ) + ( 0 ) + (( ci)); ( co) = ( ci) ? __d <= ( 0 ) : __d < ( 0 ); (v) = __d; } while (0);
      recurse(ADD_CIO, n_values,  (0x20 + 0) ,  (0x20 + 0) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET | CY_0);
      do { word __d = ( 0 ) - ( 0 ) - (( ci)); ( co) = ( ci) ? __d >= ( 0 ) : __d > ( 0 ); (v) = __d; } while (0);
      recurse(SUB_CIO, n_values,  (0x20 + 0) ,  (0x20 + 0) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
      do { word __d = ( 0 ) - ( -1 ) - (( ci)); ( co) = ( ci) ? __d >= ( 0 ) : __d > ( 0 ); (v) = __d; } while (0);
      recurse(SUB_CIO, n_values,  (0x20 + 0) ,  (0x20 + -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  CY_JUST_SET | CY_1);
      do { word __d = ( 0 ) + ( -1 ) + (( ci)); ( co) = ( ci) ? __d <= ( 0 ) : __d < ( 0 ); (v) = __d; } while (0);
      recurse(ADD_CIO, n_values,  (0x20 + 0) ,  (0x20 + -1) , v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

    }

  if (allowed_cost > 1)
    {
      ((v) = ( 0x80000000 ), ( co) = ( ci));
      recurse(COPY, n_values,  (0x20 - 2) ,  0, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

      ((v) = ( -1 ), ( co) = ( ci));
      recurse(COPY, n_values,  (0x20 + -1) ,  0, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);

      ((v) = ( 1 ), ( co) = ( ci));
      recurse(COPY, n_values,  (0x20 + 1) ,  0, v, 1, sequence, n_insns, values, n_values + 1, goal_value, allowed_cost, co,  prune_hint & ~CY_JUST_SET);
    }
}
