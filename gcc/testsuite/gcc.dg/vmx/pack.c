#include "harness.h"

#define BIG 4294967295

static void test()
{
  /* Input vectors.  */
  vector unsigned short vusa = {0,1,2,3,4,5,6,7};
  vector unsigned short vusb = {8,9,10,11,12,13,14,15};
  vector signed short vssa = {-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed short vssb = {0,1,2,3,4,5,6,7};
  vector bool short vbsa = {0,65535,65535,0,0,0,65535,0};
  vector bool short vbsb = {65535,0,0,65535,65535,65535,0,65535};
  vector unsigned int vuia = {0,1,2,3};
  vector unsigned int vuib = {4,5,6,7};
  vector signed int vsia = {-4,-3,-2,-1};
  vector signed int vsib = {0,1,2,3};
  vector bool int vbia = {0,BIG,BIG,BIG};
  vector bool int vbib = {BIG,0,0,0};
  vector unsigned int vipa = {(0<<24) + (2<<19) + (3<<11) + (4<<3),
			      (1<<24) + (5<<19) + (6<<11) + (7<<3),
			      (0<<24) + (8<<19) + (9<<11) + (10<<3),
			      (1<<24) + (11<<19) + (12<<11) + (13<<3)};
  vector unsigned int vipb = {(1<<24) + (14<<19) + (15<<11) + (16<<3),
			      (0<<24) + (17<<19) + (18<<11) + (19<<3),
			      (1<<24) + (20<<19) + (21<<11) + (22<<3),
			      (0<<24) + (23<<19) + (24<<11) + (25<<3)};
  vector unsigned short vusc = {0,256,1,257,2,258,3,259};
  vector unsigned short vusd = {4,260,5,261,6,262,7,263};
  vector signed short vssc = {-1,-128,0,127,-2,-129,1,128};
  vector signed short vssd = {-3,-130,2,129,-4,-131,3,130};
  vector unsigned int vuic = {0,65536,1,65537};
  vector unsigned int vuid = {2,65538,3,65539};
  vector signed int vsic = {-1,-32768,0,32767};
  vector signed int vsid = {-2,-32769,1,32768};

  /* Result vectors.  */
  vector unsigned char vucr;
  vector signed char vscr;
  vector bool char vbcr;
  vector unsigned short vusr;
  vector signed short vssr;
  vector bool short vbsr;
  vector pixel vpr;
  vector unsigned char vucsr;
  vector signed char vscsr;
  vector unsigned short vussr;
  vector signed short vsssr;
  vector unsigned char vucsur1, vucsur2;
  vector unsigned short vussur1, vussur2;

  /* Expected result vectors.  */
  vector unsigned char vucer = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char vscer = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector bool char vbcer = {0,255,255,0,0,0,255,0,255,0,0,255,255,255,0,255};
  vector unsigned short vuser = {0,1,2,3,4,5,6,7};
  vector signed short vsser = {-4,-3,-2,-1,0,1,2,3};
  vector bool short vbser = {0,65535,65535,65535,65535,0,0,0};
  vector pixel vper = {(0<<15) + (2<<10) + (3<<5) + 4,
		       (1<<15) + (5<<10) + (6<<5) + 7,
		       (0<<15) + (8<<10) + (9<<5) + 10,
		       (1<<15) + (11<<10) + (12<<5) + 13,
		       (1<<15) + (14<<10) + (15<<5) + 16,
		       (0<<15) + (17<<10) + (18<<5) + 19,
		       (1<<15) + (20<<10) + (21<<5) + 22,
		       (0<<15) + (23<<10) + (24<<5) + 25};
  vector unsigned char vucser = {0,255,1,255,2,255,3,255,4,255,5,255,6,255,7,255};
  vector signed char vscser = {-1,-128,0,127,-2,-128,1,127,
			       -3,-128,2,127,-4,-128,3,127};
  vector unsigned short vusser = {0,65535,1,65535,2,65535,3,65535};
  vector signed short vssser = {-1,-32768,0,32767,-2,-32768,1,32767};
  vector unsigned char vucsuer1 = {0,255,1,255,2,255,3,255,4,255,5,255,6,255,7,255};
  vector unsigned char vucsuer2 = {0,0,0,127,0,0,1,128,0,0,2,129,0,0,3,130};
  vector unsigned short vussuer1 = {0,65535,1,65535,2,65535,3,65535};
  vector unsigned short vussuer2 = {0,0,0,32767,0,0,1,32768};

  vucr = vec_pack (vusa, vusb);
  vscr = vec_pack (vssa, vssb);
  vbcr = vec_pack (vbsa, vbsb);
  vusr = vec_pack (vuia, vuib);
  vssr = vec_pack (vsia, vsib);
  vbsr = vec_pack (vbia, vbib);
  vpr  = vec_packpx (vipa, vipb);
  vucsr = vec_packs (vusc, vusd);
  vscsr = vec_packs (vssc, vssd);
  vussr = vec_packs (vuic, vuid);
  vsssr = vec_packs (vsic, vsid);
  vucsur1 = vec_packsu (vusc, vusd);
  vucsur2 = vec_packsu (vssc, vssd);
  vussur1 = vec_packsu (vuic, vuid);
  vussur2 = vec_packsu (vsic, vsid);

  check (vec_all_eq (vucr, vucer), "vucr");
  check (vec_all_eq (vscr, vscer), "vscr");
  check (vec_all_eq (vbcr, vbcer), "vbcr");
  check (vec_all_eq (vusr, vuser), "vusr");
  check (vec_all_eq (vssr, vsser), "vssr");
  check (vec_all_eq (vbsr, vbser), "vbsr");
  check (vec_all_eq (vpr,  vper ), "vpr" );
  check (vec_all_eq (vucsr, vucser), "vucsr");
  check (vec_all_eq (vscsr, vscser), "vscsr");
  check (vec_all_eq (vussr, vusser), "vussr");
  check (vec_all_eq (vsssr, vssser), "vsssr");
  check (vec_all_eq (vucsur1, vucsuer1), "vucsur1");
  check (vec_all_eq (vucsur2, vucsuer2), "vucsur2");
  check (vec_all_eq (vussur1, vussuer1), "vussur1");
  check (vec_all_eq (vussur2, vussuer2), "vussur2");
}
