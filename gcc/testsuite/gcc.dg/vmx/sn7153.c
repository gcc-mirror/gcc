/* In the source code, the vec_adds appears before the call to
   validate_sat().  In the .s code, the vaddubs has been moved to after
   the call to validate_sat().  This invalidates the meaning of checking
   the saturation bit.  */

#include <stdio.h>
#include <stdlib.h>
#include <altivec.h>

static int failed;

void validate_sat();
void validate_u8(vector unsigned char, vector unsigned char);

int
main()
{
  vector unsigned char result_u8;
  vec_mtvscr(((vector unsigned short){0,0,0,0,0,0,0,0}));
  result_u8 = vec_adds(((vector unsigned
			char){0,1,2,3,0xfc,0xfd,0xfe,0xff,
			      0,1,2,3,0xfc,0xfd,0xfe,0xff}),
		       ((vector unsigned
			char){0,0xf0,0xfd,0xfd,2,2,2,2,0,
			      0xf0,0xfd,0xfd,2,2,2,2}));
  validate_sat();
  validate_u8(result_u8, ((vector unsigned
			  char){0,0xf1,0xff,0xff,0xfe,0xff,0xff,0xff,
				  0,0xf1,0xff,0xff,0xfe,0xff,0xff,0xff}));
  if (failed)
    abort ();
  return 0;
}

void validate_sat()
{
  if (vec_any_ne(vec_splat(vec_mfvscr(), 7), ((vector unsigned short){1,1,1,1,1,1,1,1})))
    {
      union {vector unsigned short v; unsigned short s[8];} u;
      u.v = vec_mfvscr();
      printf("error: vscr == { %d,%d,%d,%d,%d,%d,%d,%d }",
	     u.s[0], u.s[1], u.s[2], u.s[3],
	     u.s[4], u.s[5], u.s[6], u.s[7]);
      printf("expected { 1,1,1,1,1,1,1,1 }\n");
      failed++;
    }
}

void validate_u8(vector unsigned char v, vector unsigned char vx)
{
  union {vector unsigned char v; unsigned char x[16]; } u, ux;
  int i;
  u.v = v;
  ux.v = vx;
  for (i=0; i<16; i++) {
    if (u.x[i] != ux.x[i]) {
      printf(" error: field %d %#2.2x expected %#2.2x\n",
             i, u.x[i], ux.x[i]);
      failed++;
    }
  }
}
