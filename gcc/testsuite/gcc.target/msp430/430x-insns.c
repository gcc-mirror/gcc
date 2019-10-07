/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" "-mcpu=430" "-msmall" } { "" } } */
/* { dg-options "-O1 -mlarge" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* The purpose of this test is to check that all insn patters in msp430.md
   which use the "Yx" constraint work as expected.
   So when both of the operands are in lower memory, a 430 instruction is
   generated, and when at least one of the operands might be in upper memory,
   a 430X instruction is generated.
   We do not need to extensively test the situation where one of the operands
   in an insn is not a mem (i.e. it is a register or immediate).  A single test
   will verify that the constraint correctly assumes that a reg
   or immediate does not itself require a 430X instruction.  */

typedef char qi;
typedef int hi;
/* For insns which use the QHI mode iterator (therefore accepting QI, HI and 
   PSI modes), we also check the PSImode version.  All insns should be 430x
   in that case.  */
typedef __int20 psi;
typedef long si;

#define ATTR_EITHER __attribute__((either))
#define ATTR_LOWER __attribute__((lower))

/* Use these to generate 430X insns.  */
qi ATTR_EITHER eqi1, eqi2, eqi3, eqi4, eqi5, eqi6;
hi ATTR_EITHER ehi1, ehi2, ehi3, ehi4, ehi5, ehi6;
psi ATTR_EITHER epsi1, epsi2, epsi3, epsi4, epsi5, epsi6;
si ATTR_EITHER esi1, esi2, esi3, esi4, esi5, esi6;

/* Use these to generate 430 insns.  */
qi ATTR_LOWER lqi1, lqi2, lqi3, lqi4, lqi5, lqi6;
hi ATTR_LOWER lhi1, lhi2, lhi3, lhi4, lhi5, lhi6;
psi ATTR_LOWER lpsi1, lpsi2, lpsi3, lpsi4, lpsi5, lpsi6;
si ATTR_LOWER lsi1, lsi2, lsi3, lsi4, lsi5, lsi6;

/* The default data region is "lower", so these behave the same as the above
   "l*" variables unless -mdata-region={upper,either,none} is passed.  */
qi qi1, qi2, qi3, qi4, qi5, qi6;
hi hi1, hi2, hi3, hi4, hi5, hi6;
psi psi1, psi2, psi3, psi4, psi5, psi6;
si si1, si2, si3, si4, si5, si6;

qi use_qi(qi a);
hi use_hi(hi a);
psi use_psi(psi a);
si use_si(si a);

#define USE_MODE(MODE) use_ ## MODE
#define USE_MODE_N(MODE,N) use_ ## N ## MODE

#define E_VAR(MODE,N) e ## MODE ## N
#define L_VAR(MODE,N) l ## MODE ## N
#define VAR(MODE,N) MODE ## N

#define REG_VAR(MODE,N) MODE ## r ## N

/* Verify that a register operand does not influence whether a 430X instruction
   is used or not.  */
/*
** register_operand:  { target msp430_region_lower }
** ...
**	MOV.B	&lqi1, R12
** ...
**	MOV.B	&qi1, R12
** ...
**	MOVX.B	&eqi1, R12
** ...
*/
/*
** register_operand:  { target msp430_region_not_lower }
** ...
**	MOV.B	&lqi1, R12
** ...
**	MOVX.B	&qi1, R12
** ...
**	MOVX.B	&eqi1, R12
** ...
*/

void
register_operand (void)
{
  use_qi (lqi1);
  use_qi (qi1);
  use_qi (eqi1);
}

/* Verify that an immediate operand does not influence whether a 430X instruction
   is used or not.  */
/*
** immediate_operand: { target msp430_region_lower }
** ...
**	MOV.B	#1, &lqi1
**	MOV.B	#2, &qi1
**	MOVX.B	#3, &eqi1
** ...
*/
/*
** immediate_operand: { target msp430_region_not_lower }
** ...
**	MOV.B	#1, &lqi1
**	MOVX.B	#2, &qi1
**	MOVX.B	#3, &eqi1
** ...
*/
void
immediate_operand (void)
{
  lqi1 = 1;
  qi1 = 2;
  eqi1 = 3;
}

/* Verify that variables marked with the noinit, persistent, section and lower
   attributes have the appropriate format instructions generated to handle
   them.  */
int __attribute__((persistent)) pp = 10;
int __attribute__((noinit)) nn;
int __attribute__((section(".data.foo"))) s;
int __attribute__((section(".data.foo"),lower)) sl1;
int __attribute__((lower,section(".data.foo"))) sl2;

/*
** attrs:
** ...
**	MOVX.W	#1, &pp
**	MOV.W	#2, &nn
**	MOVX.W	#3, &s
**	MOV.W	#4, &sl1
**	MOV.W	#5, &sl2
** ...
*/
void
attrs (void)
{
  pp = 1;
  nn = 2;
  s = 3;
  sl1 = 4;
  sl2 = 5;
}

#define MOV_INSNS(MODE) \
  E_VAR(MODE, 1) = E_VAR(MODE, 2); \
  E_VAR(MODE, 3) = L_VAR(MODE, 1); \
  E_VAR(MODE, 4) = VAR(MODE, 1); \
  L_VAR(MODE, 4) = E_VAR(MODE, 5); \
  VAR(MODE, 5) = E_VAR(MODE, 6); \
  L_VAR(MODE, 2) = L_VAR(MODE, 3); \
  L_VAR(MODE, 5) = VAR(MODE, 2); \
  VAR(MODE, 3) = VAR(MODE, 4); \
  VAR(MODE, 6) = L_VAR(MODE, 6);


/*
** movqi: { target msp430_region_lower }
** ...
**	MOVX.B	&eqi2, &eqi1
**	MOVX.B	&lqi1, &eqi3
**	MOVX.B	&qi1, &eqi4
**	MOVX.B	&eqi5, &lqi4
**	MOVX.B	&eqi6, &qi5
**	MOV.B	&lqi3, &lqi2
**	MOV.B	&qi2, &lqi5
**	MOV.B	&qi4, &qi3
**	MOV.B	&lqi6, &qi6
** ...
*/
/*
** movqi: { target msp430_region_not_lower }
** ...
**	MOVX.B	&eqi2, &eqi1
**	MOVX.B	&lqi1, &eqi3
**	MOVX.B	&qi1, &eqi4
**	MOVX.B	&eqi5, &lqi4
**	MOVX.B	&eqi6, &qi5
**	MOV.B	&lqi3, &lqi2
**	MOVX.B	&qi2, &lqi5
**	MOVX.B	&qi4, &qi3
**	MOVX.B	&lqi6, &qi6
** ...
*/
void
movqi (void)
{
  MOV_INSNS (qi)
}

/*
** movhi: { target msp430_region_lower }
** ...
**	MOVX.W	&ehi2, &ehi1
**	MOVX.W	&lhi1, &ehi3
**	MOVX.W	&hi1, &ehi4
**	MOVX.W	&ehi5, &lhi4
**	MOVX.W	&ehi6, &hi5
**	MOV.W	&lhi3, &lhi2
**	MOV.W	&hi2, &lhi5
**	MOV.W	&hi4, &hi3
**	MOV.W	&lhi6, &hi6
** ...
*/
/*
** movhi: { target msp430_region_not_lower }
** ...
**	MOVX.W	&ehi2, &ehi1
**	MOVX.W	&lhi1, &ehi3
**	MOVX.W	&hi1, &ehi4
**	MOVX.W	&ehi5, &lhi4
**	MOVX.W	&ehi6, &hi5
**	MOV.W	&lhi3, &lhi2
**	MOVX.W	&hi2, &lhi5
**	MOVX.W	&hi4, &hi3
**	MOVX.W	&lhi6, &hi6
** ...
*/
void
movhi (void)
{
  MOV_INSNS (hi)
}

/* There is no specific movsi3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** movsi: { target msp430_region_lower }
** ...
**	MOVX.W	&esi2, &esi1
**	MOVX.W	&esi2\+2, &esi1\+2
**	MOVX.W	&lsi1, &esi3
**	MOVX.W	&lsi1\+2, &esi3\+2
**	MOVX.W	&si1, &esi4
**	MOVX.W	&si1\+2, &esi4\+2
**	MOVX.W	&esi5, &lsi4
**	MOVX.W	&esi5\+2, &lsi4\+2
**	MOVX.W	&esi6, &si5
**	MOVX.W	&esi6\+2, &si5\+2
**	MOV.W	&lsi3, &lsi2
**	MOV.W	&lsi3\+2, &lsi2\+2
**	MOV.W	&si2, &lsi5
**	MOV.W	&si2\+2, &lsi5\+2
**	MOV.W	&si4, &si3
**	MOV.W	&si4\+2, &si3\+2
**	MOV.W	&lsi6, &si6
**	MOV.W	&lsi6\+2, &si6\+2
** ...
*/
/*
** movsi: { target msp430_region_not_lower }
** ...
**	MOVX.W	&esi2, &esi1
**	MOVX.W	&esi2\+2, &esi1\+2
**	MOVX.W	&lsi1, &esi3
**	MOVX.W	&lsi1\+2, &esi3\+2
**	MOVX.W	&si1, &esi4
**	MOVX.W	&si1\+2, &esi4\+2
**	MOVX.W	&esi5, &lsi4
**	MOVX.W	&esi5\+2, &lsi4\+2
**	MOVX.W	&esi6, &si5
**	MOVX.W	&esi6\+2, &si5\+2
**	MOV.W	&lsi3, &lsi2
**	MOV.W	&lsi3\+2, &lsi2\+2
**	MOVX.W	&si2, &lsi5
**	MOVX.W	&si2\+2, &lsi5\+2
**	MOVX.W	&si4, &si3
**	MOVX.W	&si4\+2, &si3\+2
**	MOVX.W	&lsi6, &si6
**	MOVX.W	&lsi6\+2, &si6\+2
** ...
*/
void
movsi (void)
{
  MOV_INSNS (si)
}

#define ADD_INSNS(MODE) \
  E_VAR(MODE,1) += E_VAR(MODE,2); \
  E_VAR(MODE,3) += L_VAR(MODE,1); \
  E_VAR(MODE,4) += VAR(MODE,1); \
  L_VAR(MODE,2) += E_VAR(MODE,5); \
  VAR(MODE,3) += E_VAR(MODE,6); \
  L_VAR(MODE,3) += L_VAR(MODE,4); \
  L_VAR(MODE,5) += VAR(MODE,2); \
  VAR(MODE,4) += L_VAR(MODE,6); \
  VAR(MODE,5) += VAR(MODE,6);

/*
** addqi3: { target msp430_region_lower }
** ...
**	ADDX.B	&eqi2, &eqi1
**	ADDX.B	&lqi1, &eqi3
**	ADDX.B	&qi1, &eqi4
**	ADDX.B	&eqi5, &lqi2
**	ADDX.B	&eqi6, &qi3
**	ADD.B	&lqi4, &lqi3
**	ADD.B	&qi2, &lqi5
**	ADD.B	&lqi6, &qi4
**	ADD.B	&qi6, &qi5
** ...
*/
/*
** addqi3: { target msp430_region_not_lower }
** ...
**	ADDX.B	&eqi2, &eqi1
**	ADDX.B	&lqi1, &eqi3
**	ADDX.B	&qi1, &eqi4
**	ADDX.B	&eqi5, &lqi2
**	ADDX.B	&eqi6, &qi3
**	ADD.B	&lqi4, &lqi3
**	ADDX.B	&qi2, &lqi5
**	ADDX.B	&lqi6, &qi4
**	ADDX.B	&qi6, &qi5
** ...
*/
void
addqi3 (void)
{
  ADD_INSNS(qi)
}

/*
** addhi3: { target msp430_region_lower }
** ...
**	ADDX.W	&ehi2, &ehi1
**	ADDX.W	&lhi1, &ehi3
**	ADDX.W	&hi1, &ehi4
**	ADDX.W	&ehi5, &lhi2
**	ADDX.W	&ehi6, &hi3
**	ADD.W	&lhi4, &lhi3
**	ADD.W	&hi2, &lhi5
**	ADD.W	&lhi6, &hi4
**	ADD.W	&hi6, &hi5
** ...
*/
/*
** addhi3: { target msp430_region_not_lower }
** ...
**	ADDX.W	&ehi2, &ehi1
**	ADDX.W	&lhi1, &ehi3
**	ADDX.W	&hi1, &ehi4
**	ADDX.W	&ehi5, &lhi2
**	ADDX.W	&ehi6, &hi3
**	ADD.W	&lhi4, &lhi3
**	ADDX.W	&hi2, &lhi5
**	ADDX.W	&lhi6, &hi4
**	ADDX.W	&hi6, &hi5
** ...
*/
void
addhi3 (void)
{
  ADD_INSNS(hi)
}

/*
** addsi3: { target msp430_region_lower }
** ...
**	ADDX	&esi2, &esi1 { ADDCX	&esi2\+2, &esi1\+2
**	ADDX	&lsi1, &esi3 { ADDCX	&lsi1\+2, &esi3\+2
**	ADDX	&si1, &esi4 { ADDCX	&si1\+2, &esi4\+2
**	ADDX	&esi5, &lsi2 { ADDCX	&esi5\+2, &lsi2\+2
**	ADDX	&esi6, &si3 { ADDCX	&esi6\+2, &si3\+2
**	ADD	&lsi4, &lsi3 { ADDC	&lsi4\+2, &lsi3\+2
**	ADD	&si2, &lsi5 { ADDC	&si2\+2, &lsi5\+2
**	ADD	&lsi6, &si4 { ADDC	&lsi6\+2, &si4\+2
**	ADD	&si6, &si5 { ADDC	&si6\+2, &si5\+2
** ...
*/
/*
** addsi3: { target msp430_region_not_lower }
** ...
**	ADDX	&esi2, &esi1 { ADDCX	&esi2\+2, &esi1\+2
**	ADDX	&lsi1, &esi3 { ADDCX	&lsi1\+2, &esi3\+2
**	ADDX	&si1, &esi4 { ADDCX	&si1\+2, &esi4\+2
**	ADDX	&esi5, &lsi2 { ADDCX	&esi5\+2, &lsi2\+2
**	ADDX	&esi6, &si3 { ADDCX	&esi6\+2, &si3\+2
**	ADD	&lsi4, &lsi3 { ADDC	&lsi4\+2, &lsi3\+2
**	ADDX	&si2, &lsi5 { ADDCX	&si2\+2, &lsi5\+2
**	ADDX	&lsi6, &si4 { ADDCX	&lsi6\+2, &si4\+2
**	ADDX	&si6, &si5 { ADDCX	&si6\+2, &si5\+2
** ...
*/
void
addsi3 (void)
{
  ADD_INSNS(si)
}

#define SUB_INSNS(MODE) \
  E_VAR(MODE,1) -= E_VAR(MODE,2); \
  E_VAR(MODE,3) -= L_VAR(MODE,1); \
  E_VAR(MODE,4) -= VAR(MODE,1); \
  L_VAR(MODE,2) -= E_VAR(MODE,5); \
  VAR(MODE,3) -= E_VAR(MODE,6); \
  L_VAR(MODE,3) -= L_VAR(MODE,4); \
  L_VAR(MODE,5) -= VAR(MODE,2); \
  VAR(MODE,4) -= L_VAR(MODE,6); \
  VAR(MODE,5) -= VAR(MODE,6);

/*
** subqi3: { target msp430_region_lower }
** ...
**	SUBX.B	&eqi2, &eqi1
**	SUBX.B	&lqi1, &eqi3
**	SUBX.B	&qi1, &eqi4
**	SUBX.B	&eqi5, &lqi2
**	SUBX.B	&eqi6, &qi3
**	SUB.B	&lqi4, &lqi3
**	SUB.B	&qi2, &lqi5
**	SUB.B	&lqi6, &qi4
**	SUB.B	&qi6, &qi5
** ...
*/
/*
** subqi3: { target msp430_region_not_lower }
** ...
**	SUBX.B	&eqi2, &eqi1
**	SUBX.B	&lqi1, &eqi3
**	SUBX.B	&qi1, &eqi4
**	SUBX.B	&eqi5, &lqi2
**	SUBX.B	&eqi6, &qi3
**	SUB.B	&lqi4, &lqi3
**	SUBX.B	&qi2, &lqi5
**	SUBX.B	&lqi6, &qi4
**	SUBX.B	&qi6, &qi5
** ...
*/
void
subqi3 (void)
{
  SUB_INSNS(qi)
}

/*
** subhi3: { target msp430_region_lower }
** ...
**	SUBX.W	&ehi2, &ehi1
**	SUBX.W	&lhi1, &ehi3
**	SUBX.W	&hi1, &ehi4
**	SUBX.W	&ehi5, &lhi2
**	SUBX.W	&ehi6, &hi3
**	SUB.W	&lhi4, &lhi3
**	SUB.W	&hi2, &lhi5
**	SUB.W	&lhi6, &hi4
**	SUB.W	&hi6, &hi5
** ...
*/
/*
** subhi3: { target msp430_region_not_lower }
** ...
**	SUBX.W	&ehi2, &ehi1
**	SUBX.W	&lhi1, &ehi3
**	SUBX.W	&hi1, &ehi4
**	SUBX.W	&ehi5, &lhi2
**	SUBX.W	&ehi6, &hi3
**	SUB.W	&lhi4, &lhi3
**	SUBX.W	&hi2, &lhi5
**	SUBX.W	&lhi6, &hi4
**	SUBX.W	&hi6, &hi5
** ...
*/
void
subhi3 (void)
{
  SUB_INSNS(hi)
}

/*
** subsi3: { target msp430_region_lower }
** ...
**	SUBX	&esi2, &esi1 { SUBCX	&esi2\+2, &esi1\+2
**	SUBX	&lsi1, &esi3 { SUBCX	&lsi1\+2, &esi3\+2
**	SUBX	&si1, &esi4 { SUBCX	&si1\+2, &esi4\+2
**	SUBX	&esi5, &lsi2 { SUBCX	&esi5\+2, &lsi2\+2
**	SUBX	&esi6, &si3 { SUBCX	&esi6\+2, &si3\+2
**	SUB	&lsi4, &lsi3 { SUBC	&lsi4\+2, &lsi3\+2
**	SUB	&si2, &lsi5 { SUBC	&si2\+2, &lsi5\+2
**	SUB	&lsi6, &si4 { SUBC	&lsi6\+2, &si4\+2
**	SUB	&si6, &si5 { SUBC	&si6\+2, &si5\+2
** ...
*/
/*
** subsi3: { target msp430_region_not_lower }
** ...
**	SUBX	&esi2, &esi1 { SUBCX	&esi2\+2, &esi1\+2
**	SUBX	&lsi1, &esi3 { SUBCX	&lsi1\+2, &esi3\+2
**	SUBX	&si1, &esi4 { SUBCX	&si1\+2, &esi4\+2
**	SUBX	&esi5, &lsi2 { SUBCX	&esi5\+2, &lsi2\+2
**	SUBX	&esi6, &si3 { SUBCX	&esi6\+2, &si3\+2
**	SUB	&lsi4, &lsi3 { SUBC	&lsi4\+2, &lsi3\+2
**	SUBX	&si2, &lsi5 { SUBCX	&si2\+2, &lsi5\+2
**	SUBX	&lsi6, &si4 { SUBCX	&lsi6\+2, &si4\+2
**	SUBX	&si6, &si5 { SUBCX	&si6\+2, &si5\+2
** ...
*/
void
subsi3 (void)
{
  SUB_INSNS(si)
}

#define BIC_INSN(MODE) \
  E_VAR(MODE,1) &= (E_VAR(MODE,2) ^ E_VAR(MODE,1)); \
  E_VAR(MODE,3) &= (L_VAR(MODE,1) ^ E_VAR(MODE,3)); \
  E_VAR(MODE,4) &= (VAR(MODE,1) ^ E_VAR(MODE,4)); \
  L_VAR(MODE,2) &= (E_VAR(MODE,5) ^ L_VAR(MODE,2)); \
  VAR(MODE,2) &= (E_VAR(MODE,6) ^ VAR(MODE,2)); \
  L_VAR(MODE,3) &= (L_VAR(MODE,4) ^ L_VAR(MODE,3)); \
  L_VAR(MODE,5) &= (VAR(MODE,3) ^ L_VAR(MODE,5)); \
  VAR(MODE,4) &= (L_VAR(MODE,6) ^ VAR(MODE,4)); \
  VAR(MODE,5) &= (VAR(MODE,6) ^ VAR(MODE,5)); \

/*
** bicqi3: { target msp430_region_lower }
** ...
**	BICX.B	&eqi2, &eqi1
**	BICX.B	&lqi1, &eqi3
**	BICX.B	&qi1, &eqi4
**	BICX.B	&eqi5, &lqi2
**	BICX.B	&eqi6, &qi2
**	BIC.B	&lqi4, &lqi3
**	BIC.B	&qi3, &lqi5
**	BIC.B	&lqi6, &qi4
**	BIC.B	&qi6, &qi5
** ...
*/
/*
** bicqi3: { target msp430_region_not_lower }
** ...
**	BICX.B	&eqi2, &eqi1
**	BICX.B	&lqi1, &eqi3
**	BICX.B	&qi1, &eqi4
**	BICX.B	&eqi5, &lqi2
**	BICX.B	&eqi6, &qi2
**	BIC.B	&lqi4, &lqi3
**	BICX.B	&qi3, &lqi5
**	BICX.B	&lqi6, &qi4
**	BICX.B	&qi6, &qi5
** ...
*/
void
bicqi3 (void)
{
  BIC_INSN(qi)
}

/*
** bichi3: { target msp430_region_lower }
** ...
**	BICX.W	&ehi2, &ehi1
**	BICX.W	&lhi1, &ehi3
**	BICX.W	&hi1, &ehi4
**	BICX.W	&ehi5, &lhi2
**	BICX.W	&ehi6, &hi2
**	BIC.W	&lhi4, &lhi3
**	BIC.W	&hi3, &lhi5
**	BIC.W	&lhi6, &hi4
**	BIC.W	&hi6, &hi5
** ...
*/
/*
** bichi3: { target msp430_region_not_lower }
** ...
**	BICX.W	&ehi2, &ehi1
**	BICX.W	&lhi1, &ehi3
**	BICX.W	&hi1, &ehi4
**	BICX.W	&ehi5, &lhi2
**	BICX.W	&ehi6, &hi2
**	BIC.W	&lhi4, &lhi3
**	BICX.W	&hi3, &lhi5
**	BICX.W	&lhi6, &hi4
**	BICX.W	&hi6, &hi5
** ...
*/
void
bichi3 (void)
{
  BIC_INSN(hi)
}

/*
** bicpsi3:
** ...
**	BICX.A	&epsi2, &epsi1
**	BICX.A	&lpsi1, &epsi3
**	BICX.A	&psi1, &epsi4
**	BICX.A	&epsi5, &lpsi2
**	BICX.A	&epsi6, &psi2
**	BICX.A	&lpsi4, &lpsi3
**	BICX.A	&psi3, &lpsi5
**	BICX.A	&lpsi6, &psi4
**	BICX.A	&psi6, &psi5
** ...
*/
void
bicpsi3 (void)
{
  BIC_INSN(psi)
}

/* There is no specific bicsi3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** bicsi3: { target msp430_region_lower }
** ...
**	BICX.W	&esi2, &esi1
**	BICX.W	&esi2\+2, &esi1\+2
**	BICX.W	&lsi1, &esi3
**	BICX.W	&lsi1\+2, &esi3\+2
**	BICX.W	&si1, &esi4
**	BICX.W	&si1\+2, &esi4\+2
**	BICX.W	&esi5, &lsi2
**	BICX.W	&esi5\+2, &lsi2\+2
**	BICX.W	&esi6, &si2
**	BICX.W	&esi6\+2, &si2\+2
**	BIC.W	&lsi4, &lsi3
**	BIC.W	&lsi4\+2, &lsi3\+2
**	BIC.W	&si3, &lsi5
**	BIC.W	&si3\+2, &lsi5\+2
**	BIC.W	&lsi6, &si4
**	BIC.W	&lsi6\+2, &si4\+2
**	BIC.W	&si6, &si5
**	BIC.W	&si6\+2, &si5\+2
** ...
*/
/*
** bicsi3: { target msp430_region_not_lower }
** ...
**	BICX.W	&esi2, &esi1
**	BICX.W	&esi2\+2, &esi1\+2
**	BICX.W	&lsi1, &esi3
**	BICX.W	&lsi1\+2, &esi3\+2
**	BICX.W	&si1, &esi4
**	BICX.W	&si1\+2, &esi4\+2
**	BICX.W	&esi5, &lsi2
**	BICX.W	&esi5\+2, &lsi2\+2
**	BICX.W	&esi6, &si2
**	BICX.W	&esi6\+2, &si2\+2
**	BIC.W	&lsi4, &lsi3
**	BIC.W	&lsi4\+2, &lsi3\+2
**	BICX.W	&si3, &lsi5
**	BICX.W	&si3\+2, &lsi5\+2
**	BICX.W	&lsi6, &si4
**	BICX.W	&lsi6\+2, &si4\+2
**	BICX.W	&si6, &si5
**	BICX.W	&si6\+2, &si5\+2
** ...
*/
void
bicsi3 (void)
{
  BIC_INSN(si)
}

#define BIC_CG_INSN(MODE) \
  E_VAR(MODE,1) &= (1 ^ E_VAR(MODE,1)); \
  E_VAR(MODE,2) &= (2 ^ E_VAR(MODE,2)); \
  L_VAR(MODE,1) &= (4 ^ L_VAR(MODE,1)); \
  VAR(MODE,1) &= (8 ^ VAR(MODE,1)); \

/*
** bic_cg_qi3: { target msp430_region_lower }
** ...
**	BICX.B	#1, &eqi1
**	BICX.B	#2, &eqi2
**	BIC.B	#4, &lqi1
**	BIC.B	#8, &qi1
** ...
*/
/*
** bic_cg_qi3: { target msp430_region_not_lower }
** ...
**	BICX.B	#1, &eqi1
**	BICX.B	#2, &eqi2
**	BIC.B	#4, &lqi1
**	BICX.B	#8, &qi1
** ...
*/
void
bic_cg_qi3 (void)
{
  BIC_CG_INSN(qi)
}

/*
** bic_cg_hi3: { target msp430_region_lower }
** ...
**	BICX.W	#1, &ehi1
**	BICX.W	#2, &ehi2
**	BIC.W	#4, &lhi1
**	BIC.W	#8, &hi1
** ...
*/
/*
** bic_cg_hi3: { target msp430_region_not_lower }
** ...
**	BICX.W	#1, &ehi1
**	BICX.W	#2, &ehi2
**	BIC.W	#4, &lhi1
**	BICX.W	#8, &hi1
** ...
*/
void
bic_cg_hi3 (void)
{
  BIC_CG_INSN(hi)
}

/*
** bic_cg_psi3:
** ...
**	BICX.A	#1, &epsi1
**	BICX.A	#2, &epsi2
**	BICX.A	#4, &lpsi1
**	BICX.A	#8, &psi1
** ...
*/
void
bic_cg_psi3 (void)
{
  BIC_CG_INSN(psi)
}

/* There is no specific bic_cg_si3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** bic_cg_si3: { target msp430_region_lower }
** ...
**	BICX.W	#1, &esi1
**	BICX.W	#2, &esi2
**	BIC.W	#4, &lsi1
**	BIC.W	#8, &si1
** ...
*/
/*
** bic_cg_si3: { target msp430_region_not_lower }
** ...
**	BICX.W	#1, &esi1
**	BICX.W	#2, &esi2
**	BIC.W	#4, &lsi1
**	BICX.W	#8, &si1
** ...
*/
void
bic_cg_si3 (void)
{
  BIC_CG_INSN(si)
}

#define AND_INSN(MODE) \
  E_VAR(MODE,1) &= E_VAR(MODE,2); \
  E_VAR(MODE,3) &= L_VAR(MODE,1); \
  E_VAR(MODE,4) &= VAR(MODE,1); \
  L_VAR(MODE,2) &= E_VAR(MODE,5); \
  VAR(MODE,2) &= E_VAR(MODE,6); \
  L_VAR(MODE,3) &= L_VAR(MODE,4); \
  L_VAR(MODE,5) &= VAR(MODE,3); \
  VAR(MODE,4) &= VAR(MODE,5); \
  VAR(MODE,6) &= L_VAR(MODE,6);

/*
** andqi3: { target msp430_region_lower }
** ...
**	ANDX.B	&eqi2, &eqi1
**	ANDX.B	&lqi1, &eqi3
**	ANDX.B	&qi1, &eqi4
**	ANDX.B	&eqi5, &lqi2
**	ANDX.B	&eqi6, &qi2
**	AND.B	&lqi4, &lqi3
**	AND.B	&qi3, &lqi5
**	AND.B	&qi5, &qi4
**	AND.B	&lqi6, &qi6
** ...
*/
/*
** andqi3: { target msp430_region_not_lower }
** ...
**	ANDX.B	&eqi2, &eqi1
**	ANDX.B	&lqi1, &eqi3
**	ANDX.B	&qi1, &eqi4
**	ANDX.B	&eqi5, &lqi2
**	ANDX.B	&eqi6, &qi2
**	AND.B	&lqi4, &lqi3
**	ANDX.B	&qi3, &lqi5
**	ANDX.B	&qi5, &qi4
**	ANDX.B	&lqi6, &qi6
** ...
*/
void
andqi3 (void)
{
  AND_INSN(qi)
}

/*
** andhi3: { target msp430_region_lower }
** ...
**	ANDX.W	&ehi2, &ehi1
**	ANDX.W	&lhi1, &ehi3
**	ANDX.W	&hi1, &ehi4
**	ANDX.W	&ehi5, &lhi2
**	ANDX.W	&ehi6, &hi2
**	AND.W	&lhi4, &lhi3
**	AND.W	&hi3, &lhi5
**	AND.W	&hi5, &hi4
**	AND.W	&lhi6, &hi6
** ...
*/
/*
** andhi3: { target msp430_region_not_lower }
** ...
**	ANDX.W	&ehi2, &ehi1
**	ANDX.W	&lhi1, &ehi3
**	ANDX.W	&hi1, &ehi4
**	ANDX.W	&ehi5, &lhi2
**	ANDX.W	&ehi6, &hi2
**	AND.W	&lhi4, &lhi3
**	ANDX.W	&hi3, &lhi5
**	ANDX.W	&hi5, &hi4
**	ANDX.W	&lhi6, &hi6
** ...
*/
void
andhi3 (void)
{
  AND_INSN(hi)
}

/*
** andpsi3:
** ...
**	ANDX.A	&epsi2, &epsi1
**	ANDX.A	&lpsi1, &epsi3
**	ANDX.A	&psi1, &epsi4
**	ANDX.A	&epsi5, &lpsi2
**	ANDX.A	&epsi6, &psi2
**	ANDX.A	&lpsi4, &lpsi3
**	ANDX.A	&psi3, &lpsi5
**	ANDX.A	&psi5, &psi4
**	ANDX.A	&lpsi6, &psi6
** ...
*/
void
andpsi3 (void)
{
  AND_INSN(psi)
}

/* There is no specific andsi3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** andsi3: { target msp430_region_lower }
** ...
**	ANDX.W	&esi2, &esi1
**	ANDX.W	&esi2\+2, &esi1\+2
**	ANDX.W	&lsi1, &esi3
**	ANDX.W	&lsi1\+2, &esi3\+2
**	ANDX.W	&si1, &esi4
**	ANDX.W	&si1\+2, &esi4\+2
**	ANDX.W	&esi5, &lsi2
**	ANDX.W	&esi5\+2, &lsi2\+2
**	ANDX.W	&esi6, &si2
**	ANDX.W	&esi6\+2, &si2\+2
**	AND.W	&lsi4, &lsi3
**	AND.W	&lsi4\+2, &lsi3\+2
**	AND.W	&si3, &lsi5
**	AND.W	&si3\+2, &lsi5\+2
**	AND.W	&si5, &si4
**	AND.W	&si5\+2, &si4\+2
**	AND.W	&lsi6, &si6
**	AND.W	&lsi6\+2, &si6\+2
** ...
*/
/*
** andsi3: { target msp430_region_not_lower }
** ...
**	ANDX.W	&esi2, &esi1
**	ANDX.W	&esi2\+2, &esi1\+2
**	ANDX.W	&lsi1, &esi3
**	ANDX.W	&lsi1\+2, &esi3\+2
**	ANDX.W	&si1, &esi4
**	ANDX.W	&si1\+2, &esi4\+2
**	ANDX.W	&esi5, &lsi2
**	ANDX.W	&esi5\+2, &lsi2\+2
**	ANDX.W	&esi6, &si2
**	ANDX.W	&esi6\+2, &si2\+2
**	AND.W	&lsi4, &lsi3
**	AND.W	&lsi4\+2, &lsi3\+2
**	ANDX.W	&si3, &lsi5
**	ANDX.W	&si3\+2, &lsi5\+2
**	ANDX.W	&si5, &si4
**	ANDX.W	&si5\+2, &si4\+2
**	ANDX.W	&lsi6, &si6
**	ANDX.W	&lsi6\+2, &si6\+2
** ...
*/
void
andsi3 (void)
{
  AND_INSN(si)
}

#define IOR_INSN(MODE) \
  E_VAR(MODE,1) |= E_VAR(MODE,2); \
  E_VAR(MODE,3) |= L_VAR(MODE,1); \
  E_VAR(MODE,4) |= VAR(MODE,1); \
  L_VAR(MODE,2) |= E_VAR(MODE,5); \
  VAR(MODE,2) |= E_VAR(MODE,6); \
  L_VAR(MODE,3) |= L_VAR(MODE,4); \
  L_VAR(MODE,5) |= VAR(MODE,3); \
  VAR(MODE,4) |= VAR(MODE,5); \
  VAR(MODE,6) |= L_VAR(MODE,6);

/*
** iorqi3: { target msp430_region_lower }
** ...
**	BISX.B	&eqi2, &eqi1
**	BISX.B	&lqi1, &eqi3
**	BISX.B	&qi1, &eqi4
**	BISX.B	&eqi5, &lqi2
**	BISX.B	&eqi6, &qi2
**	BIS.B	&lqi4, &lqi3
**	BIS.B	&qi3, &lqi5
**	BIS.B	&qi5, &qi4
**	BIS.B	&lqi6, &qi6
** ...
*/
/*
** iorqi3: { target msp430_region_not_lower }
** ...
**	BISX.B	&eqi2, &eqi1
**	BISX.B	&lqi1, &eqi3
**	BISX.B	&qi1, &eqi4
**	BISX.B	&eqi5, &lqi2
**	BISX.B	&eqi6, &qi2
**	BIS.B	&lqi4, &lqi3
**	BISX.B	&qi3, &lqi5
**	BISX.B	&qi5, &qi4
**	BISX.B	&lqi6, &qi6
** ...
*/
void
iorqi3 (void)
{
  IOR_INSN(qi)
}

/*
** iorhi3: { target msp430_region_lower }
** ...
**	BISX.W	&ehi2, &ehi1
**	BISX.W	&lhi1, &ehi3
**	BISX.W	&hi1, &ehi4
**	BISX.W	&ehi5, &lhi2
**	BISX.W	&ehi6, &hi2
**	BIS.W	&lhi4, &lhi3
**	BIS.W	&hi3, &lhi5
**	BIS.W	&hi5, &hi4
**	BIS.W	&lhi6, &hi6
** ...
*/
/*
** iorhi3: { target msp430_region_not_lower }
** ...
**	BISX.W	&ehi2, &ehi1
**	BISX.W	&lhi1, &ehi3
**	BISX.W	&hi1, &ehi4
**	BISX.W	&ehi5, &lhi2
**	BISX.W	&ehi6, &hi2
**	BIS.W	&lhi4, &lhi3
**	BISX.W	&hi3, &lhi5
**	BISX.W	&hi5, &hi4
**	BISX.W	&lhi6, &hi6
** ...
*/
void
iorhi3 (void)
{
  IOR_INSN(hi)
}

/*
** iorpsi3:
** ...
**	BISX.A	&epsi2, &epsi1
**	BISX.A	&lpsi1, &epsi3
**	BISX.A	&psi1, &epsi4
**	BISX.A	&epsi5, &lpsi2
**	BISX.A	&epsi6, &psi2
**	BISX.A	&lpsi4, &lpsi3
**	BISX.A	&psi3, &lpsi5
**	BISX.A	&psi5, &psi4
**	BISX.A	&lpsi6, &psi6
** ...
*/
void
iorpsi3 (void)
{
  IOR_INSN(psi)
}

/* There is no specific iorsi3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** iorsi3: { target msp430_region_lower }
** ...
**	BISX.W	&esi2, &esi1
**	BISX.W	&esi2\+2, &esi1\+2
**	BISX.W	&lsi1, &esi3
**	BISX.W	&lsi1\+2, &esi3\+2
**	BISX.W	&si1, &esi4
**	BISX.W	&si1\+2, &esi4\+2
**	BISX.W	&esi5, &lsi2
**	BISX.W	&esi5\+2, &lsi2\+2
**	BISX.W	&esi6, &si2
**	BISX.W	&esi6\+2, &si2\+2
**	BIS.W	&lsi4, &lsi3
**	BIS.W	&lsi4\+2, &lsi3\+2
**	BIS.W	&si3, &lsi5
**	BIS.W	&si3\+2, &lsi5\+2
**	BIS.W	&si5, &si4
**	BIS.W	&si5\+2, &si4\+2
**	BIS.W	&lsi6, &si6
**	BIS.W	&lsi6\+2, &si6\+2
** ...
*/
/*
** iorsi3: { target msp430_region_not_lower }
** ...
**	BISX.W	&esi2, &esi1
**	BISX.W	&esi2\+2, &esi1\+2
**	BISX.W	&lsi1, &esi3
**	BISX.W	&lsi1\+2, &esi3\+2
**	BISX.W	&si1, &esi4
**	BISX.W	&si1\+2, &esi4\+2
**	BISX.W	&esi5, &lsi2
**	BISX.W	&esi5\+2, &lsi2\+2
**	BISX.W	&esi6, &si2
**	BISX.W	&esi6\+2, &si2\+2
**	BIS.W	&lsi4, &lsi3
**	BIS.W	&lsi4\+2, &lsi3\+2
**	BISX.W	&si3, &lsi5
**	BISX.W	&si3\+2, &lsi5\+2
**	BISX.W	&si5, &si4
**	BISX.W	&si5\+2, &si4\+2
**	BISX.W	&lsi6, &si6
**	BISX.W	&lsi6\+2, &si6\+2
** ...
*/
void
iorsi3 (void)
{
  IOR_INSN(si)
}

#define XOR_INSN(MODE) \
  E_VAR(MODE,1) ^= E_VAR(MODE,2); \
  E_VAR(MODE,3) ^= L_VAR(MODE,1); \
  E_VAR(MODE,4) ^= VAR(MODE,1); \
  L_VAR(MODE,2) ^= E_VAR(MODE,5); \
  VAR(MODE,2) ^= E_VAR(MODE,6); \
  L_VAR(MODE,3) ^= L_VAR(MODE,4); \
  L_VAR(MODE,5) ^= VAR(MODE,3); \
  VAR(MODE,4) ^= VAR(MODE,5); \
  VAR(MODE,6) ^= L_VAR(MODE,6);

/*
** xorqi3: { target msp430_region_lower }
** ...
**	XORX.B	&eqi2, &eqi1
**	XORX.B	&lqi1, &eqi3
**	XORX.B	&qi1, &eqi4
**	XORX.B	&eqi5, &lqi2
**	XORX.B	&eqi6, &qi2
**	XOR.B	&lqi4, &lqi3
**	XOR.B	&qi3, &lqi5
**	XOR.B	&qi5, &qi4
**	XOR.B	&lqi6, &qi6
** ...
*/
/*
** xorqi3: { target msp430_region_not_lower }
** ...
**	XORX.B	&eqi2, &eqi1
**	XORX.B	&lqi1, &eqi3
**	XORX.B	&qi1, &eqi4
**	XORX.B	&eqi5, &lqi2
**	XORX.B	&eqi6, &qi2
**	XOR.B	&lqi4, &lqi3
**	XORX.B	&qi3, &lqi5
**	XORX.B	&qi5, &qi4
**	XORX.B	&lqi6, &qi6
** ...
*/
void
xorqi3 (void)
{
  XOR_INSN(qi)
}

/*
** xorhi3: { target msp430_region_lower }
** ...
**	XORX.W	&ehi2, &ehi1
**	XORX.W	&lhi1, &ehi3
**	XORX.W	&hi1, &ehi4
**	XORX.W	&ehi5, &lhi2
**	XORX.W	&ehi6, &hi2
**	XOR.W	&lhi4, &lhi3
**	XOR.W	&hi3, &lhi5
**	XOR.W	&hi5, &hi4
**	XOR.W	&lhi6, &hi6
** ...
*/
/*
** xorhi3: { target msp430_region_not_lower }
** ...
**	XORX.W	&ehi2, &ehi1
**	XORX.W	&lhi1, &ehi3
**	XORX.W	&hi1, &ehi4
**	XORX.W	&ehi5, &lhi2
**	XORX.W	&ehi6, &hi2
**	XOR.W	&lhi4, &lhi3
**	XORX.W	&hi3, &lhi5
**	XORX.W	&hi5, &hi4
**	XORX.W	&lhi6, &hi6
** ...
*/
void
xorhi3 (void)
{
  XOR_INSN(hi)
}

/*
** xorpsi3:
** ...
**	XORX.A	&epsi2, &epsi1
**	XORX.A	&lpsi1, &epsi3
**	XORX.A	&psi1, &epsi4
**	XORX.A	&epsi5, &lpsi2
**	XORX.A	&epsi6, &psi2
**	XORX.A	&lpsi4, &lpsi3
**	XORX.A	&psi3, &lpsi5
**	XORX.A	&psi5, &psi4
**	XORX.A	&lpsi6, &psi6
** ...
*/
void
xorpsi3 (void)
{
  XOR_INSN(psi)
}

/* There is no specific xorsi3 pattern defined for msp430, but we check
   this is synthesized correctly anyway.  */
/*
** xorsi3: { target msp430_region_lower }
** ...
**	XORX.W	&esi2, &esi1
**	XORX.W	&esi2\+2, &esi1\+2
**	XORX.W	&lsi1, &esi3
**	XORX.W	&lsi1\+2, &esi3\+2
**	XORX.W	&si1, &esi4
**	XORX.W	&si1\+2, &esi4\+2
**	XORX.W	&esi5, &lsi2
**	XORX.W	&esi5\+2, &lsi2\+2
**	XORX.W	&esi6, &si2
**	XORX.W	&esi6\+2, &si2\+2
**	XOR.W	&lsi4, &lsi3
**	XOR.W	&lsi4\+2, &lsi3\+2
**	XOR.W	&si3, &lsi5
**	XOR.W	&si3\+2, &lsi5\+2
**	XOR.W	&si5, &si4
**	XOR.W	&si5\+2, &si4\+2
**	XOR.W	&lsi6, &si6
**	XOR.W	&lsi6\+2, &si6\+2
** ...
*/
/*
** xorsi3: { target msp430_region_not_lower }
** ...
**	XORX.W	&esi2, &esi1
**	XORX.W	&esi2\+2, &esi1\+2
**	XORX.W	&lsi1, &esi3
**	XORX.W	&lsi1\+2, &esi3\+2
**	XORX.W	&si1, &esi4
**	XORX.W	&si1\+2, &esi4\+2
**	XORX.W	&esi5, &lsi2
**	XORX.W	&esi5\+2, &lsi2\+2
**	XORX.W	&esi6, &si2
**	XORX.W	&esi6\+2, &si2\+2
**	XOR.W	&lsi4, &lsi3
**	XOR.W	&lsi4\+2, &lsi3\+2
**	XORX.W	&si3, &lsi5
**	XORX.W	&si3\+2, &lsi5\+2
**	XORX.W	&si5, &si4
**	XORX.W	&si5\+2, &si4\+2
**	XORX.W	&lsi6, &si6
**	XORX.W	&lsi6\+2, &si6\+2
** ...
*/
void
xorsi3 (void)
{
  XOR_INSN(si)
}

#define DO1 \
{ \
  qi z; \
  z += use_qi(z); \
  use_qi(z); \
}

#define DO2 \
{ \
  hi z; \
  z += use_hi(z); \
  use_hi(z); \
}

#define DO3 \
{ \
  si z; \
  z += use_si(z); \
  use_si(z); \
}

#define CBRANCH_INSN(MODE) \
  if (E_VAR(MODE,1) == E_VAR(MODE,2)) \
    DO1 \
  else if (E_VAR(MODE,3) == L_VAR(MODE,1)) \
    DO2 \
  else if (E_VAR(MODE,4) == VAR(MODE,1)) \
    DO1 \
  else if (L_VAR(MODE,2) == E_VAR(MODE,5)) \
    DO2 \
  else if (VAR(MODE,2) == E_VAR(MODE,6)) \
    DO1 \
  else if (L_VAR(MODE,3) == L_VAR(MODE,4)) \
    DO2 \
  else if (L_VAR(MODE,5) == VAR(MODE,3)) \
    DO2 \
  else if (VAR(MODE,4) == VAR(MODE,5)) \
    DO1 \
  else if (VAR(MODE,6) == L_VAR(MODE,6)) \
    DO2

/*
** cbranchqi4_real: { target msp430_region_lower }
** ...
**	CMPX.B	&eqi2, &eqi1 { JEQ	.L[0-9]+
**	CMPX.B	&lqi1, &eqi3 { JEQ	.L[0-9]+
**	CMPX.B	&qi1, &eqi4 { JEQ	.L[0-9]+
**	CMPX.B	&eqi5, &lqi2 { JEQ	.L[0-9]+
**	CMPX.B	&eqi6, &qi2 { JEQ	.L[0-9]+
**	CMP.B	&lqi4, &lqi3 { JEQ	.L[0-9]+
**	CMP.B	&qi3, &lqi5 { JEQ	.L[0-9]+
**	CMP.B	&qi5, &qi4 { JEQ	.L[0-9]+
**	CMP.B	&lqi6, &qi6 { JNE	.L[0-9]+
** ...
*/
/*
** cbranchqi4_real: { target msp430_region_not_lower }
** ...
**	CMPX.B	&eqi2, &eqi1 { JEQ	.L[0-9]+
**	CMPX.B	&lqi1, &eqi3 { JEQ	.L[0-9]+
**	CMPX.B	&qi1, &eqi4 { JEQ	.L[0-9]+
**	CMPX.B	&eqi5, &lqi2 { JEQ	.L[0-9]+
**	CMPX.B	&eqi6, &qi2 { JEQ	.L[0-9]+
**	CMP.B	&lqi4, &lqi3 { JEQ	.L[0-9]+
**	CMPX.B	&qi3, &lqi5 { JEQ	.L[0-9]+
**	CMPX.B	&qi5, &qi4 { JEQ	.L[0-9]+
**	CMPX.B	&lqi6, &qi6 { JNE	.L[0-9]+
** ...
*/
void
cbranchqi4_real (void)
{
  CBRANCH_INSN(qi)
}

/*
** cbranchhi4_real: { target msp430_region_lower }
** ...
**	CMPX.W	&ehi2, &ehi1 { JEQ	.L[0-9]+
**	CMPX.W	&lhi1, &ehi3 { JEQ	.L[0-9]+
**	CMPX.W	&hi1, &ehi4 { JEQ	.L[0-9]+
**	CMPX.W	&ehi5, &lhi2 { JEQ	.L[0-9]+
**	CMPX.W	&ehi6, &hi2 { JEQ	.L[0-9]+
**	CMP.W	&lhi4, &lhi3 { JEQ	.L[0-9]+
**	CMP.W	&hi3, &lhi5 { JEQ	.L[0-9]+
**	CMP.W	&hi5, &hi4 { JEQ	.L[0-9]+
**	CMP.W	&lhi6, &hi6 { JNE	.L[0-9]+
** ...
*/
/*
** cbranchhi4_real: { target msp430_region_not_lower }
** ...
**	CMPX.W	&ehi2, &ehi1 { JEQ	.L[0-9]+
**	CMPX.W	&lhi1, &ehi3 { JEQ	.L[0-9]+
**	CMPX.W	&hi1, &ehi4 { JEQ	.L[0-9]+
**	CMPX.W	&ehi5, &lhi2 { JEQ	.L[0-9]+
**	CMPX.W	&ehi6, &hi2 { JEQ	.L[0-9]+
**	CMP.W	&lhi4, &lhi3 { JEQ	.L[0-9]+
**	CMPX.W	&hi3, &lhi5 { JEQ	.L[0-9]+
**	CMPX.W	&hi5, &hi4 { JEQ	.L[0-9]+
**	CMPX.W	&lhi6, &hi6 { JNE	.L[0-9]+
** ...
*/
void
cbranchhi4_real (void)
{
  CBRANCH_INSN(hi)
}

/* There is no specific cbranchsi4_real pattern defined for msp430, but we
   check this is synthesized correctly anyway.  */
/*
** cbranchsi4_real: { target msp430_region_lower }
** ...
**	CMPX.W	&esi2, &esi1 { JEQ	.L[0-9]+
**	CMPX.W	&lsi1, &esi3 { JEQ	.L[0-9]+
**	CMPX.W	&si1, &esi4 { JEQ	.L[0-9]+
**	CMPX.W	&esi5, &lsi2 { JEQ	.L[0-9]+
**	CMPX.W	&esi6, &si2 { JEQ	.L[0-9]+
**	CMP.W	&lsi4, &lsi3 { JEQ	.L[0-9]+
**	CMP.W	&si3, &lsi5 { JEQ	.L[0-9]+
**	CMP.W	&si5, &si4 { JEQ	.L[0-9]+
**	CMP.W	&lsi6, &si6 { JNE	.L[0-9]+
**	CMP.W	&lsi6\+2, &si6\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi2\+2, &esi1\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&lsi1\+2, &esi3\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&si1\+2, &esi4\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi5\+2, &lsi2\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi6\+2, &si2\+2 { JNE	.L[0-9]+
** ...
**	CMP.W	&lsi4\+2, &lsi3\+2 { JNE	.L[0-9]+
** ...
**	CMP.W	&si3\+2, &lsi5\+2 { JNE	.L[0-9]+
** ...
**	CMP.W	&si5\+2, &si4\+2 { JNE	.L[0-9]+
** ...
*/
/*
** cbranchsi4_real: { target msp430_region_not_lower }
** ...
**	CMPX.W	&esi2, &esi1 { JEQ	.L[0-9]+
**	CMPX.W	&lsi1, &esi3 { JEQ	.L[0-9]+
**	CMPX.W	&si1, &esi4 { JEQ	.L[0-9]+
**	CMPX.W	&esi5, &lsi2 { JEQ	.L[0-9]+
**	CMPX.W	&esi6, &si2 { JEQ	.L[0-9]+
**	CMP.W	&lsi4, &lsi3 { JEQ	.L[0-9]+
**	CMPX.W	&si3, &lsi5 { JEQ	.L[0-9]+
**	CMPX.W	&si5, &si4 { JEQ	.L[0-9]+
**	CMPX.W	&lsi6, &si6 { JNE	.L[0-9]+
**	CMPX.W	&lsi6\+2, &si6\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi2\+2, &esi1\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&lsi1\+2, &esi3\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&si1\+2, &esi4\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi5\+2, &lsi2\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&esi6\+2, &si2\+2 { JNE	.L[0-9]+
** ...
**	CMP.W	&lsi4\+2, &lsi3\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&si3\+2, &lsi5\+2 { JNE	.L[0-9]+
** ...
**	CMPX.W	&si5\+2, &si4\+2 { JNE	.L[0-9]+
** ...
*/
void
cbranchsi4_real (void)
{
  CBRANCH_INSN(si)
}

#define CBRANCH_REVERSE_INSN(MODE) \
  if (E_VAR(MODE,1) > E_VAR(MODE,2)) \
    DO1 \
  else if (E_VAR(MODE,3) > L_VAR(MODE,1)) \
    DO2 \
  else if (E_VAR(MODE,4) > VAR(MODE,1)) \
    DO1 \
  else if (L_VAR(MODE,2) > E_VAR(MODE,5)) \
    DO2 \
  else if (VAR(MODE,2) > E_VAR(MODE,6)) \
    DO1 \
  else if (L_VAR(MODE,3) > L_VAR(MODE,4)) \
    DO2 \
  else if (L_VAR(MODE,5) > VAR(MODE,3)) \
    DO2 \
  else if (VAR(MODE,4) > VAR(MODE,5)) \
    DO1 \
  else if (VAR(MODE,6) > L_VAR(MODE,6)) \
    DO2

/*
** cbranchqi4_reversed: { target msp430_region_lower }
** ...
**	CMPX.B	&eqi1, &eqi2 { JLO	.L[0-9]+
**	CMPX.B	&eqi3, &lqi1 { JLO	.L[0-9]+
**	CMPX.B	&eqi4, &qi1 { JLO	.L[0-9]+
**	CMPX.B	&lqi2, &eqi5 { JLO	.L[0-9]+
**	CMPX.B	&qi2, &eqi6 { JLO	.L[0-9]+
**	CMP.B	&lqi3, &lqi4 { JLO	.L[0-9]+
**	CMP.B	&lqi5, &qi3 { JLO	.L[0-9]+
**	CMP.B	&qi4, &qi5 { JLO	.L[0-9]+
**	CMP.B	&qi6, &lqi6 { JHS	.L[0-9]+
** ...
*/
/*
** cbranchqi4_reversed: { target msp430_region_not_lower }
** ...
**	CMPX.B	&eqi1, &eqi2 { JLO	.L[0-9]+
**	CMPX.B	&eqi3, &lqi1 { JLO	.L[0-9]+
**	CMPX.B	&eqi4, &qi1 { JLO	.L[0-9]+
**	CMPX.B	&lqi2, &eqi5 { JLO	.L[0-9]+
**	CMPX.B	&qi2, &eqi6 { JLO	.L[0-9]+
**	CMP.B	&lqi3, &lqi4 { JLO	.L[0-9]+
**	CMPX.B	&lqi5, &qi3 { JLO	.L[0-9]+
**	CMPX.B	&qi4, &qi5 { JLO	.L[0-9]+
**	CMPX.B	&qi6, &lqi6 { JHS	.L[0-9]+
** ...
*/
void
cbranchqi4_reversed (void)
{
  CBRANCH_REVERSE_INSN(qi)
}

/*
** cbranchhi4_reversed: { target msp430_region_lower }
** ...
**	CMPX.W	&ehi1, &ehi2 { JL	.L[0-9]+
**	CMPX.W	&ehi3, &lhi1 { JL	.L[0-9]+
**	CMPX.W	&ehi4, &hi1 { JL	.L[0-9]+
**	CMPX.W	&lhi2, &ehi5 { JL	.L[0-9]+
**	CMPX.W	&hi2, &ehi6 { JL	.L[0-9]+
**	CMP.W	&lhi3, &lhi4 { JL	.L[0-9]+
**	CMP.W	&lhi5, &hi3 { JL	.L[0-9]+
**	CMP.W	&hi4, &hi5 { JL	.L[0-9]+
**	CMP.W	&hi6, &lhi6 { JGE	.L[0-9]+
** ...
*/
/*
** cbranchhi4_reversed: { target msp430_region_not_lower }
** ...
**	CMPX.W	&ehi1, &ehi2 { JL	.L[0-9]+
**	CMPX.W	&ehi3, &lhi1 { JL	.L[0-9]+
**	CMPX.W	&ehi4, &hi1 { JL	.L[0-9]+
**	CMPX.W	&lhi2, &ehi5 { JL	.L[0-9]+
**	CMPX.W	&hi2, &ehi6 { JL	.L[0-9]+
**	CMP.W	&lhi3, &lhi4 { JL	.L[0-9]+
**	CMPX.W	&lhi5, &hi3 { JL	.L[0-9]+
**	CMPX.W	&hi4, &hi5 { JL	.L[0-9]+
**	CMPX.W	&hi6, &lhi6 { JGE	.L[0-9]+
** ...
*/
void
cbranchhi4_reversed (void)
{
  CBRANCH_REVERSE_INSN(hi)
}

/* There is no specific cbranchsi4_reversed pattern defined for msp430, but
   we check this is synthesized correctly anyway.
   This output assembly for this one is quite long and convoluted so we only
   check part of it.  */
/*
** cbranchsi4_reversed: { target msp430_region_lower }
** ...
**	CMPX.W	&esi1\+2, &esi2\+2 { JL	.L[0-9]+
**	CMPX.W	&esi2\+2, &esi1\+2 { JEQ	.L[0-9]+
**	CMPX.W	&esi3\+2, &lsi1\+2 { JL	.L[0-9]+
**	CMPX.W	&lsi1\+2, &esi3\+2 { JEQ	.L[0-9]+
**	CMPX.W	&esi4\+2, &si1\+2 { JL	.L[0-9]+
**	CMPX.W	&si1\+2, &esi4\+2 { JEQ	.L[0-9]+
**	CMPX.W	&lsi2\+2, &esi5\+2 { JL	.L[0-9]+
**	CMPX.W	&esi5\+2, &lsi2\+2 { JEQ	.L[0-9]+
**	CMPX.W	&si2\+2, &esi6\+2 { JL	.L[0-9]+
**	CMPX.W	&esi6\+2, &si2\+2 { JEQ	.L[0-9]+
**	CMP.W	&lsi3\+2, &lsi4\+2 { JL	.L[0-9]+
**	CMP.W	&lsi4\+2, &lsi3\+2 { JEQ	.L[0-9]+
**	CMP.W	&lsi5\+2, &si3\+2 { JL	.L[0-9]+
**	CMP.W	&si3\+2, &lsi5\+2 { JEQ	.L[0-9]+
**	CMP.W	&si4\+2, &si5\+2 { JL	.L[0-9]+
**	CMP.W	&si5\+2, &si4\+2 { JEQ	.L[0-9]+
**	CMP.W	&si6\+2, &lsi6\+2 { JL	.L[0-9]+
**	CMP.W	&lsi6\+2, &si6\+2 { JNE	.L[0-9]+
** ...
*/
/*
** cbranchsi4_reversed: { target msp430_region_not_lower }
** ...
**	CMPX.W	&esi1\+2, &esi2\+2 { JL	.L[0-9]+
**	CMPX.W	&esi2\+2, &esi1\+2 { JEQ	.L[0-9]+
**	CMPX.W	&esi3\+2, &lsi1\+2 { JL	.L[0-9]+
**	CMPX.W	&lsi1\+2, &esi3\+2 { JEQ	.L[0-9]+
**	CMPX.W	&esi4\+2, &si1\+2 { JL	.L[0-9]+
**	CMPX.W	&si1\+2, &esi4\+2 { JEQ	.L[0-9]+
**	CMPX.W	&lsi2\+2, &esi5\+2 { JL	.L[0-9]+
**	CMPX.W	&esi5\+2, &lsi2\+2 { JEQ	.L[0-9]+
**	CMPX.W	&si2\+2, &esi6\+2 { JL	.L[0-9]+
**	CMPX.W	&esi6\+2, &si2\+2 { JEQ	.L[0-9]+
**	CMP.W	&lsi3\+2, &lsi4\+2 { JL	.L[0-9]+
**	CMP.W	&lsi4\+2, &lsi3\+2 { JEQ	.L[0-9]+
**	CMPX.W	&lsi5\+2, &si3\+2 { JL	.L[0-9]+
**	CMPX.W	&si3\+2, &lsi5\+2 { JEQ	.L[0-9]+
**	CMPX.W	&si4\+2, &si5\+2 { JL	.L[0-9]+
**	CMPX.W	&si5\+2, &si4\+2 { JEQ	.L[0-9]+
**	CMPX.W	&si6\+2, &lsi6\+2 { JL	.L[0-9]+
**	CMPX.W	&lsi6\+2, &si6\+2 { JNE	.L[0-9]+
** ...
*/
void
cbranchsi4_reversed (void)
{
  CBRANCH_REVERSE_INSN(si)
}

#define BITBRANCH_NE_INSN(MODE) \
  if (E_VAR(MODE,1) & E_VAR(MODE,2)) \
    DO1 \
  else if (E_VAR(MODE,3) & L_VAR(MODE,1)) \
    DO2 \
  else if (E_VAR(MODE,4) & VAR(MODE,1)) \
    DO1 \
  else if (L_VAR(MODE,2) & E_VAR(MODE,5)) \
    DO2 \
  else if (VAR(MODE,2) & E_VAR(MODE,6)) \
    DO1 \
  else if (L_VAR(MODE,3) & L_VAR(MODE,4)) \
    DO2 \
  else if (L_VAR(MODE,5) & VAR(MODE,3)) \
    DO2 \
  else if (VAR(MODE,4) & VAR(MODE,5)) \
    DO1 \
  else if (VAR(MODE,6) & L_VAR(MODE,6)) \
    DO2
/*
** bitbranchqi4: { target msp430_region_lower }
** ...
**	BITX.B	&eqi2, &eqi1 { JNE	.L[0-9]+
**	BITX.B	&lqi1, &eqi3 { JNE	.L[0-9]+
**	BITX.B	&qi1, &eqi4 { JNE	.L[0-9]+
**	BITX.B	&eqi5, &lqi2 { JNE	.L[0-9]+
**	BITX.B	&eqi6, &qi2 { JNE	.L[0-9]+
**	BIT.B	&lqi4, &lqi3 { JNE	.L[0-9]+
**	BIT.B	&qi3, &lqi5 { JNE	.L[0-9]+
**	BIT.B	&qi5, &qi4 { JNE	.L[0-9]+
**	BIT.B	&lqi6, &qi6 { JEQ	.L[0-9]+
** ...
*/
/*
** bitbranchqi4: { target msp430_region_not_lower }
** ...
**	BITX.B	&eqi2, &eqi1 { JNE	.L[0-9]+
**	BITX.B	&lqi1, &eqi3 { JNE	.L[0-9]+
**	BITX.B	&qi1, &eqi4 { JNE	.L[0-9]+
**	BITX.B	&eqi5, &lqi2 { JNE	.L[0-9]+
**	BITX.B	&eqi6, &qi2 { JNE	.L[0-9]+
**	BIT.B	&lqi4, &lqi3 { JNE	.L[0-9]+
**	BITX.B	&qi3, &lqi5 { JNE	.L[0-9]+
**	BITX.B	&qi5, &qi4 { JNE	.L[0-9]+
**	BITX.B	&lqi6, &qi6 { JEQ	.L[0-9]+
** ...
*/
void
bitbranchqi4 (void)
{
  BITBRANCH_NE_INSN(qi)
}

/*
** bitbranchhi4: { target msp430_region_lower }
** ...
**	BITX.W	&ehi2, &ehi1 { JNE	.L[0-9]+
**	BITX.W	&lhi1, &ehi3 { JNE	.L[0-9]+
**	BITX.W	&hi1, &ehi4 { JNE	.L[0-9]+
**	BITX.W	&ehi5, &lhi2 { JNE	.L[0-9]+
**	BITX.W	&ehi6, &hi2 { JNE	.L[0-9]+
**	BIT.W	&lhi4, &lhi3 { JNE	.L[0-9]+
**	BIT.W	&hi3, &lhi5 { JNE	.L[0-9]+
**	BIT.W	&hi5, &hi4 { JNE	.L[0-9]+
**	BIT.W	&lhi6, &hi6 { JEQ	.L[0-9]+
** ...
*/
/*
** bitbranchhi4: { target msp430_region_not_lower }
** ...
**	BITX.W	&ehi2, &ehi1 { JNE	.L[0-9]+
**	BITX.W	&lhi1, &ehi3 { JNE	.L[0-9]+
**	BITX.W	&hi1, &ehi4 { JNE	.L[0-9]+
**	BITX.W	&ehi5, &lhi2 { JNE	.L[0-9]+
**	BITX.W	&ehi6, &hi2 { JNE	.L[0-9]+
**	BIT.W	&lhi4, &lhi3 { JNE	.L[0-9]+
**	BITX.W	&hi3, &lhi5 { JNE	.L[0-9]+
**	BITX.W	&hi5, &hi4 { JNE	.L[0-9]+
**	BITX.W	&lhi6, &hi6 { JEQ	.L[0-9]+
** ...
*/
void
bitbranchhi4 (void)
{
  BITBRANCH_NE_INSN(hi)
}

/*
** bitbranchpsi4:
** ...
**	BITX.A	&epsi2, &epsi1 { JNE	.L[0-9]+
**	BITX.A	&lpsi1, &epsi3 { JNE	.L[0-9]+
**	BITX.A	&psi1, &epsi4 { JNE	.L[0-9]+
**	BITX.A	&epsi5, &lpsi2 { JNE	.L[0-9]+
**	BITX.A	&epsi6, &psi2 { JNE	.L[0-9]+
**	BITX.A	&lpsi4, &lpsi3 { JNE	.L[0-9]+
**	BITX.A	&psi3, &lpsi5 { JNE	.L[0-9]+
**	BITX.A	&psi5, &psi4 { JNE	.L[0-9]+
**	BITX.A	&lpsi6, &psi6 { JEQ	.L[0-9]+
** ...
*/
void
bitbranchpsi4 (void)
{
  BITBRANCH_NE_INSN(psi)
}


/* "bitbranch" using SImode operands is omitted since the resulting assembly
   uses many temporary registers to perform the bitwise and comparison
   operations.  */
