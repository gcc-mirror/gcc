/* PR tree-optimization/71625 - missing strlen optimization on different
   array initialization style

   Verify that strlen() of braced initialized array is folded
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-gimple -fdump-tree-optimized" } */

#include "strlenopt.h"

#define S								\
  "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f"	\
  "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"	\
  "\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f"	\
  "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f"	\
  "\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f"	\
  "\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f"	\
  "\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f"	\
  "\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f"	\
  "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f"	\
  "\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f"	\
  "\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf"	\
  "\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf"	\
  "\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf"	\
  "\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf"	\
  "\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef"	\
  "\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"

/* Arrays of char, signed char, and unsigned char to verify that
   the length and contents of all are the same as that of the string
   literal above.  */

const char c256[] = {
  S[0], S[1], S[2], S[3], S[4], S[5], S[6], S[7], S[8], S[9], S[10],
  S[11], S[12], S[13], S[14], S[15], S[16], S[17], S[18], S[19], S[20],
  S[21], S[22], S[23], S[24], S[25], S[26], S[27], S[28], S[29], S[30],
  S[31], S[32], S[33], S[34], S[35], S[36], S[37], S[38], S[39], S[40],
  S[41], S[42], S[43], S[44], S[45], S[46], S[47], S[48], S[49], S[50],
  S[51], S[52], S[53], S[54], S[55], S[56], S[57], S[58], S[59], S[60],
  S[61], S[62], S[63], S[64], S[65], S[66], S[67], S[68], S[69], S[70],
  S[71], S[72], S[73], S[74], S[75], S[76], S[77], S[78], S[79], S[80],
  S[81], S[82], S[83], S[84], S[85], S[86], S[87], S[88], S[89], S[90],
  S[91], S[92], S[93], S[94], S[95], S[96], S[97], S[98], S[99], S[100],
  S[101], S[102], S[103], S[104], S[105], S[106], S[107], S[108], S[109],
  S[110], S[111], S[112], S[113], S[114], S[115], S[116], S[117], S[118],
  S[119], S[120], S[121], S[122], S[123], S[124], S[125], S[126], S[127],
  S[128], S[129], S[130], S[131], S[132], S[133], S[134], S[135], S[136],
  S[137], S[138], S[139], S[140], S[141], S[142], S[143], S[144], S[145],
  S[146], S[147], S[148], S[149], S[150], S[151], S[152], S[153], S[154],
  S[155], S[156], S[157], S[158], S[159], S[160], S[161], S[162], S[163],
  S[164], S[165], S[166], S[167], S[168], S[169], S[170], S[171], S[172],
  S[173], S[174], S[175], S[176], S[177], S[178], S[179], S[180], S[181],
  S[182], S[183], S[184], S[185], S[186], S[187], S[188], S[189], S[190],
  S[191], S[192], S[193], S[194], S[195], S[196], S[197], S[198], S[199],
  S[200], S[201], S[202], S[203], S[204], S[205], S[206], S[207], S[208],
  S[209], S[210], S[211], S[212], S[213], S[214], S[215], S[216], S[217],
  S[218], S[219], S[220], S[221], S[222], S[223], S[224], S[225], S[226],
  S[227], S[228], S[229], S[230], S[231], S[232], S[233], S[234], S[235],
  S[236], S[237], S[238], S[239], S[240], S[241], S[242], S[243], S[244],
  S[245], S[246], S[247], S[248], S[249], S[250], S[251], S[252], S[253],
  S[254], S[255] /* = NUL */
};

const signed char sc256[] = {
  S[0], S[1], S[2], S[3], S[4], S[5], S[6], S[7], S[8], S[9], S[10],
  S[11], S[12], S[13], S[14], S[15], S[16], S[17], S[18], S[19], S[20],
  S[21], S[22], S[23], S[24], S[25], S[26], S[27], S[28], S[29], S[30],
  S[31], S[32], S[33], S[34], S[35], S[36], S[37], S[38], S[39], S[40],
  S[41], S[42], S[43], S[44], S[45], S[46], S[47], S[48], S[49], S[50],
  S[51], S[52], S[53], S[54], S[55], S[56], S[57], S[58], S[59], S[60],
  S[61], S[62], S[63], S[64], S[65], S[66], S[67], S[68], S[69], S[70],
  S[71], S[72], S[73], S[74], S[75], S[76], S[77], S[78], S[79], S[80],
  S[81], S[82], S[83], S[84], S[85], S[86], S[87], S[88], S[89], S[90],
  S[91], S[92], S[93], S[94], S[95], S[96], S[97], S[98], S[99], S[100],
  S[101], S[102], S[103], S[104], S[105], S[106], S[107], S[108], S[109],
  S[110], S[111], S[112], S[113], S[114], S[115], S[116], S[117], S[118],
  S[119], S[120], S[121], S[122], S[123], S[124], S[125], S[126], S[127],
  S[128], S[129], S[130], S[131], S[132], S[133], S[134], S[135], S[136],
  S[137], S[138], S[139], S[140], S[141], S[142], S[143], S[144], S[145],
  S[146], S[147], S[148], S[149], S[150], S[151], S[152], S[153], S[154],
  S[155], S[156], S[157], S[158], S[159], S[160], S[161], S[162], S[163],
  S[164], S[165], S[166], S[167], S[168], S[169], S[170], S[171], S[172],
  S[173], S[174], S[175], S[176], S[177], S[178], S[179], S[180], S[181],
  S[182], S[183], S[184], S[185], S[186], S[187], S[188], S[189], S[190],
  S[191], S[192], S[193], S[194], S[195], S[196], S[197], S[198], S[199],
  S[200], S[201], S[202], S[203], S[204], S[205], S[206], S[207], S[208],
  S[209], S[210], S[211], S[212], S[213], S[214], S[215], S[216], S[217],
  S[218], S[219], S[220], S[221], S[222], S[223], S[224], S[225], S[226],
  S[227], S[228], S[229], S[230], S[231], S[232], S[233], S[234], S[235],
  S[236], S[237], S[238], S[239], S[240], S[241], S[242], S[243], S[244],
  S[245], S[246], S[247], S[248], S[249], S[250], S[251], S[252], S[253],
  S[254], S[255] /* = NUL */
};

const unsigned char uc256[] = {
  S[0], S[1], S[2], S[3], S[4], S[5], S[6], S[7], S[8], S[9], S[10],
  S[11], S[12], S[13], S[14], S[15], S[16], S[17], S[18], S[19], S[20],
  S[21], S[22], S[23], S[24], S[25], S[26], S[27], S[28], S[29], S[30],
  S[31], S[32], S[33], S[34], S[35], S[36], S[37], S[38], S[39], S[40],
  S[41], S[42], S[43], S[44], S[45], S[46], S[47], S[48], S[49], S[50],
  S[51], S[52], S[53], S[54], S[55], S[56], S[57], S[58], S[59], S[60],
  S[61], S[62], S[63], S[64], S[65], S[66], S[67], S[68], S[69], S[70],
  S[71], S[72], S[73], S[74], S[75], S[76], S[77], S[78], S[79], S[80],
  S[81], S[82], S[83], S[84], S[85], S[86], S[87], S[88], S[89], S[90],
  S[91], S[92], S[93], S[94], S[95], S[96], S[97], S[98], S[99], S[100],
  S[101], S[102], S[103], S[104], S[105], S[106], S[107], S[108], S[109],
  S[110], S[111], S[112], S[113], S[114], S[115], S[116], S[117], S[118],
  S[119], S[120], S[121], S[122], S[123], S[124], S[125], S[126], S[127],
  S[128], S[129], S[130], S[131], S[132], S[133], S[134], S[135], S[136],
  S[137], S[138], S[139], S[140], S[141], S[142], S[143], S[144], S[145],
  S[146], S[147], S[148], S[149], S[150], S[151], S[152], S[153], S[154],
  S[155], S[156], S[157], S[158], S[159], S[160], S[161], S[162], S[163],
  S[164], S[165], S[166], S[167], S[168], S[169], S[170], S[171], S[172],
  S[173], S[174], S[175], S[176], S[177], S[178], S[179], S[180], S[181],
  S[182], S[183], S[184], S[185], S[186], S[187], S[188], S[189], S[190],
  S[191], S[192], S[193], S[194], S[195], S[196], S[197], S[198], S[199],
  S[200], S[201], S[202], S[203], S[204], S[205], S[206], S[207], S[208],
  S[209], S[210], S[211], S[212], S[213], S[214], S[215], S[216], S[217],
  S[218], S[219], S[220], S[221], S[222], S[223], S[224], S[225], S[226],
  S[227], S[228], S[229], S[230], S[231], S[232], S[233], S[234], S[235],
  S[236], S[237], S[238], S[239], S[240], S[241], S[242], S[243], S[244],
  S[245], S[246], S[247], S[248], S[249], S[250], S[251], S[252], S[253],
  S[254], S[255] /* = NUL */
};

const __CHAR16_TYPE__ c16_4[] = {
  1, 0x7fff, 0x8000, 0xffff,
  0x10000   /* { dg-warning "\\\[-Woverflow]" } */
};

const char a2_implicit[2] = { };
const char a3_implicit[3] = { };

const char a3_nul[3] = { 0 };
const char a5_nul1[3] = { [1] = 0 };
const char a7_nul2[3] = { [2] = 0 };

const char ax_2_nul[] = { '1', '2', '\0' };
const char ax_3_nul[] = { '1', '2', '3', '\0' };

const char ax_3_des_nul[] = { [3] = 0, [2] = '3', [1] = '2', [0] = '1' };

const char ax_3[] = { '1', '2', '3' };
const char a3_3[3] = { '1', '2', '3' };

const char ax_100_3[] = { '1', '2', '3', [100] = '\0' };

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name) CAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to funcation named
   call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr)							\
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

#define T(s, n) ELIM (strlen (s) == n)

void test_nulstring (void)
{
  T (a2_implicit, 0);
  T (a3_implicit, 0);

  T (a3_nul, 0);
  T (a5_nul1, 0);
  T (a7_nul2, 0);

  T (ax_2_nul, 2);
  T (ax_3_nul, 3);
  T (ax_3_des_nul, 3);

  T (ax_100_3, 3);
  T (ax_100_3 + 4, 0);
  ELIM (101 == sizeof ax_100_3);
  ELIM ('\0' == ax_100_3[100]);

  /* Verify that all three character arrays have the same length
     as the string literal they are initialized with.  */
  T (S, 255);
  T (c256, 255);
  T ((const char*)sc256, 255);
  T ((const char*)uc256, 255);

  /* Verify that all three character arrays have the same contents
     as the string literal they are initialized with.  */
  ELIM (0 == memcmp (c256, S, sizeof c256));
  ELIM (0 == memcmp (c256, (const char*)sc256, sizeof c256));
  ELIM (0 == memcmp (c256, (const char*)uc256, sizeof c256));

  ELIM (0 == strcmp (c256, (const char*)sc256));
  ELIM (0 == strcmp (c256, (const char*)uc256));

  /* Verify that the char16_t array has the expected contents.  */
  ELIM (c16_4[0] == 1 && c16_4[1] == 0x7fff
	&& c16_4[2] == 0x8000 && c16_4[3] == 0xffff
	&& c16_4[4] == 0);
}

/* Verify that excessively large initializers don't run out of
   memory.  Also verify that the they have the expected size and
   contents.  */

#define MAX (__PTRDIFF_MAX__ - 1)

const char large_string[] = { 'a', [1234] = 'b', [MAX] = '\0' };

const void test_large_string_size (void)
{
  ELIM (sizeof large_string == MAX + 1);

  /* The following expressions are not folded without optimization.  */
  ELIM ('a'  == large_string[0]);
  ELIM ('\0' == large_string[1233]);
  ELIM ('b'  == large_string[1234]);
  ELIM ('\0' == large_string[1235]);
  ELIM ('\0' == large_string[MAX - 1]);
}


/* { dg-final { scan-tree-dump-times "strlen1" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "memcmp" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "strcmp" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated" 0 "optimized" } } */
