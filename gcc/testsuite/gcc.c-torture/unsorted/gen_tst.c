/* Compiler Test Generator Program.
   Copyright (C) 1989 FSF.  */


#define E0 ((type *)10000000)
#define reg0 r0
#define indreg0 (*p0)
#define imm0 22
#define limm0 ((type)(int)&glob0)
#define adr0 (*E0)
#define adrreg0 (p0[10000000])
#define adrx0 (E0[x0])
#define regx0 (p0[x0])

#define E1 ((type *)11111111)
#define reg1 r1
#define indreg1 (*p1)
#define imm1 33
#define limm1 ((type)(int)&glob1)
#define adr1 (*E1)
#define adrreg1 (p1[1111111/4])
#define adrx1 (E1[x1])
#define regx1 (p1[x1])

int glob0, glob1;

#define type double

char *a0[] = {"reg0", "indreg0", "imm0", "limm0",
		"adr0", "adrreg0", "adrx0", "regx0"};
char *a1[] = {"reg1", "indreg1", "imm1", "limm1",
		"adr1", "adrreg1", "adrx1", "regx1"};

main_compare ()
{
  int i0, i1;

  for (i0 = 0;  i0 < 8;  i0++)
    {
      for (i1 = 0;  i1 < 8;  i1++)
	{
	  printf ("%s%s_cmp (r0, r1, x0, x1, p0, p1)\n", a0[i0], a1[i1]);
	  printf ("type r0, r1;  type *p0, *p1;\n");
	  printf ("{if (%s <= %s) return 1; else return 0;}\n\n",
		  a0[i0], a1[i1], a0[i0]);
	}
    }
}

main_assign ()
{
  int i0, i1;

  for (i0 = 0;  i0 < 8;  i0++)
    {
      if (i0 < 2 || i0 > 3)
      for (i1 = 0;  i1 < 8;  i1++)
	{
	  printf ("%s%s_set (r0, r1, x0, x1, p0, p1)\n", a0[i0], a1[i1]);
	  printf ("type r0, r1;  type *p0, *p1;\n");
	  printf ("{%s = %s;  }\n\n",
		  a0[i0], a1[i1]);
	}
    }
}

main () {main_assign ();}
