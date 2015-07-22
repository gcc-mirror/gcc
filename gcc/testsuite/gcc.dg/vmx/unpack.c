/* { dg-additional-options "-Wno-shift-overflow" } */

#include "harness.h"

#define BIG 4294967295

static void test()
{
  /* Input vectors.  */
  vector signed char vsc = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector bool char vbc = {0,255,255,0,0,0,255,0,255,0,0,255,255,255,0,255};
  vector pixel vp = {(0<<15) + (1<<10)  + (2<<5)  + 3,
		     (1<<15) + (4<<10)  + (5<<5)  + 6,
		     (0<<15) + (7<<10)  + (8<<5)  + 9,
		     (1<<15) + (10<<10) + (11<<5) + 12,
		     (1<<15) + (13<<10) + (14<<5) + 15,
		     (0<<15) + (16<<10) + (17<<5) + 18,
		     (1<<15) + (19<<10) + (20<<5) + 21,
		     (0<<15) + (22<<10) + (23<<5) + 24};
  vector signed short vss = {-4,-3,-2,-1,0,1,2,3};
  vector bool short vbs = {0,65535,65535,0,0,0,65535,0};

  /* Result vectors.  */
  vector signed short vsch, vscl;
  vector bool short vbsh, vbsl;
  vector unsigned int vuih, vuil;
  vector signed int vsih, vsil;
  vector bool int vbih, vbil;

  /* Expected result vectors.  */
  vector signed short vschr = {-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed short vsclr = {0,1,2,3,4,5,6,7};
  vector bool short vbshr = {0,65535,65535,0,0,0,65535,0};
  vector bool short vbslr = {65535,0,0,65535,65535,65535,0,65535};
  vector unsigned int vuihr = {(0<<24)     + (1<<16)  + (2<<8)  + 3,
			       (65535<<24) + (4<<16)  + (5<<8)  + 6,
			       (0<<24)     + (7<<16)  + (8<<8)  + 9,
			       (65535<<24) + (10<<16) + (11<<8) + 12};
  vector unsigned int vuilr = {(65535<<24) + (13<<16) + (14<<8) + 15,
			       (0<<24)     + (16<<16) + (17<<8) + 18,
			       (65535<<24) + (19<<16) + (20<<8) + 21,
			       (0<<24)     + (22<<16) + (23<<8) + 24};
  vector signed int vsihr = {-4,-3,-2,-1};
  vector signed int vsilr = {0,1,2,3};
  vector bool int vbihr = {0,BIG,BIG,0};
  vector bool int vbilr = {0,0,BIG,0};

  vsch = vec_unpackh (vsc);
  vscl = vec_unpackl (vsc);
  vbsh = vec_unpackh (vbc);
  vbsl = vec_unpackl (vbc);
  vuih = vec_unpackh (vp);
  vuil = vec_unpackl (vp);
  vsih = vec_unpackh (vss);
  vsil = vec_unpackl (vss);
  vbih = vec_unpackh (vbs);
  vbil = vec_unpackl (vbs);

  check (vec_all_eq (vsch, vschr), "vsch");
  check (vec_all_eq (vscl, vsclr), "vscl");
  check (vec_all_eq (vbsh, vbshr), "vbsh");
  check (vec_all_eq (vbsl, vbslr), "vbsl");
  check (vec_all_eq (vuih, vuihr), "vuih");
  check (vec_all_eq (vuil, vuilr), "vuil");
  check (vec_all_eq (vsih, vsihr), "vsih");
  check (vec_all_eq (vsil, vsilr), "vsil");
  check (vec_all_eq (vbih, vbihr), "vbih");
  check (vec_all_eq (vbil, vbilr), "vbil");
}
