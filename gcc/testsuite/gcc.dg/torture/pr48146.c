/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

static unsigned char
safe_sub_func_int_s_s (int si1, unsigned char si2)
{
  return si1 ^ si2 & -si2 ^ si2 ? : si1 - si2;
}

int g_2[10] = {
    0x90AC204EL
};

volatile unsigned char g_39;

unsigned char
func_67 (unsigned short p_68)
{
  unsigned char l_92;
  unsigned char l_74;
  int *l = &g_2[6];
lbl_90:*l ^= 1;
       if (p_68)
	 goto lbl_93;
       for (l_74 = 0;; l_74 = safe_sub_func_int_s_s (l_74, 1))
	 {
	   if (l_74)
	     goto lbl_90;
lbl_93:l_92 ^= 0 != &g_39;
       if (0)
	 {
	 }
       else
	 *l = 1;
	 }
}
