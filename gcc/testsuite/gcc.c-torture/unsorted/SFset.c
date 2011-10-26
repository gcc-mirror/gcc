#define E0 ((type *)10000000)
#define reg0 r0
#define indreg0 (*p0)
#define imm0 22
#define limm0 ((type)(int)&glob0)
#define adr0 (*E0)
#define adrreg0 (p0[10000000])
#define adrx0 (E0[x0])
#define regx0 (p0[x0])

#define E1 ((type *)(11111111 & ~(__alignof__ (type) - 1)))
#define reg1 r1
#define indreg1 (*p1)
#define imm1 33
#define limm1 ((type)(int)&glob1)
#define adr1 (*E1)
#define adrreg1 (p1[1111111/4])
#define adrx1 (E1[x1])
#define regx1 (p1[x1])

int glob0, glob1;

#define type float

reg0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = reg1;  }

reg0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = indreg1;  }

reg0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = imm1;  }

reg0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = limm1;  }

reg0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = adr1;  }

reg0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = adrreg1;  }

reg0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = adrx1;  }

reg0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{reg0 = regx1;  }

indreg0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = reg1;  }

indreg0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = indreg1;  }

indreg0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = imm1;  }

indreg0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = limm1;  }

indreg0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = adr1;  }

indreg0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = adrreg1;  }

indreg0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = adrx1;  }

indreg0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{indreg0 = regx1;  }

adr0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = reg1;  }

adr0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = indreg1;  }

adr0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = imm1;  }

adr0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = limm1;  }

adr0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = adr1;  }

adr0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = adrreg1;  }

adr0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = adrx1;  }

adr0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adr0 = regx1;  }

adrreg0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = reg1;  }

adrreg0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = indreg1;  }

adrreg0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = imm1;  }

adrreg0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = limm1;  }

adrreg0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = adr1;  }

adrreg0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = adrreg1;  }

adrreg0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = adrx1;  }

adrreg0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrreg0 = regx1;  }

adrx0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = reg1;  }

adrx0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = indreg1;  }

adrx0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = imm1;  }

adrx0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = limm1;  }

adrx0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = adr1;  }

adrx0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = adrreg1;  }

adrx0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = adrx1;  }

adrx0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{adrx0 = regx1;  }

regx0reg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = reg1;  }

regx0indreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = indreg1;  }

regx0imm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = imm1;  }

regx0limm1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = limm1;  }

regx0adr1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = adr1;  }

regx0adrreg1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = adrreg1;  }

regx0adrx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = adrx1;  }

regx0regx1_set (r0, r1, x0, x1, p0, p1)
type r0, r1;  type *p0, *p1;
{regx0 = regx1;  }

