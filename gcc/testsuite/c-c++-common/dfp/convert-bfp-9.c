/* { dg-xfail-run-if "" { lax_strtofp } } */
/* { dg-options "-w" } */

/* This test assumes IEEE float and double.  */

#define __STDC_WANT_DEC_FP__
#include <float.h>

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;

/* Exponent values that might cause problems with a particular
   implementation.  */

CONVERT_VALID (101, dd, df, 1.e309dd, 1.e309, 0.) 
CONVERT_VALID (102, dd, df, 1.e308dd, 1.e308, 0.)
CONVERT_VALID (103, dd, df, 1.e307dd, 1.e307, 0.) 
CONVERT_VALID (104, dd, df, 1.e306dd, 1.e306, 0.) 
CONVERT_VALID (105, dd, df, 1.e305dd, 1.e305, 0.)
CONVERT_VALID (106, dd, df, 1.e304dd, 1.e304, 0.)
CONVERT_VALID (107, dd, df, 1.e303dd, 1.e303, 0.) 
CONVERT_VALID (108, dd, df, 1.e302dd, 1.e302, 0.) 
CONVERT_VALID (109, dd, df, 1.e301dd, 1.e301, 0.) 
CONVERT_VALID (110, dd, df, 1.e300dd, 1.e300, 0.) 
CONVERT_VALID (111, dd, df, 1.e299dd, 1.e299, 0.) 
CONVERT_VALID (112, dd, df, 1.e298dd, 1.e298, 0.) 
CONVERT_VALID (113, dd, df, 1.e297dd, 1.e297, 0.) 
CONVERT_VALID (114, dd, df, 1.e296dd, 1.e296, 0.) 
CONVERT_VALID (115, dd, df, 1.e295dd, 1.e295, 0.) 
CONVERT_VALID (116, dd, df, 1.e294dd, 1.e294, 0.) 
CONVERT_VALID (117, dd, df, 1.e293dd, 1.e293, 0.) 
CONVERT_VALID (118, dd, df, 1.e292dd, 1.e292, 0.) 
CONVERT_VALID (119, dd, df, 1.e291dd, 1.e291, 0.)
CONVERT_VALID (120, dd, df, 1.e290dd, 1.e290, 0.)

CONVERT_VALID (201, dd, df, 1.e-309dd, 1.e-309, 0.) 
CONVERT_VALID (202, dd, df, 1.e-308dd, 1.e-308, 0.) 
CONVERT_VALID (203, dd, df, 1.e-307dd, 1.e-307, 0.) 
CONVERT_VALID (204, dd, df, 1.e-306dd, 1.e-306, 0.) 
CONVERT_VALID (205, dd, df, 1.e-305dd, 1.e-305, 0.) 
CONVERT_VALID (206, dd, df, 1.e-304dd, 1.e-304, 0.) 
CONVERT_VALID (207, dd, df, 1.e-303dd, 1.e-303, 0.) 
CONVERT_VALID (208, dd, df, 1.e-302dd, 1.e-302, 0.) 
CONVERT_VALID (209, dd, df, 1.e-301dd, 1.e-301, 0.) 
CONVERT_VALID (210, dd, df, 1.e-300dd, 1.e-300, 0.) 
CONVERT_VALID (211, dd, df, 1.e-299dd, 1.e-299, 0.) 
CONVERT_VALID (212, dd, df, 1.e-298dd, 1.e-298, 0.) 
CONVERT_VALID (213, dd, df, 1.e-297dd, 1.e-297, 0.) 
CONVERT_VALID (214, dd, df, 1.e-296dd, 1.e-296, 0.) 
CONVERT_VALID (215, dd, df, 1.e-295dd, 1.e-295, 0.)
CONVERT_VALID (216, dd, df, 1.e-294dd, 1.e-294, 0.)
CONVERT_VALID (217, dd, df, 1.e-293dd, 1.e-293, 0.) 
CONVERT_VALID (218, dd, df, 1.e-292dd, 1.e-292, 0.) 
CONVERT_VALID (219, dd, df, 1.e-291dd, 1.e-291, 0.) 
CONVERT_VALID (220, dd, df, 1.e-290dd, 1.e-290, 0.) 

CONVERT_VALID (301, td, df, 1.e309dl, 1.e309, 0.) 
CONVERT_VALID (302, td, df, 1.e308dl, 1.e308, 0.)
CONVERT_VALID (303, td, df, 1.e307dl, 1.e307, 0.) 
CONVERT_VALID (304, td, df, 1.e306dl, 1.e306, 0.) 
CONVERT_VALID (305, td, df, 1.e305dl, 1.e305, 0.)
CONVERT_VALID (306, td, df, 1.e304dl, 1.e304, 0.)
CONVERT_VALID (307, td, df, 1.e303dl, 1.e303, 0.) 
CONVERT_VALID (308, td, df, 1.e302dl, 1.e302, 0.) 
CONVERT_VALID (309, td, df, 1.e301dl, 1.e301, 0.)
CONVERT_VALID (310, td, df, 1.e300dl, 1.e300, 0.) 
CONVERT_VALID (311, td, df, 1.e299dl, 1.e299, 0.)
CONVERT_VALID (312, td, df, 1.e298dl, 1.e298, 0.)
CONVERT_VALID (313, td, df, 1.e297dl, 1.e297, 0.) 
CONVERT_VALID (314, td, df, 1.e296dl, 1.e296, 0.) 
CONVERT_VALID (315, td, df, 1.e295dl, 1.e295, 0.)
CONVERT_VALID (316, td, df, 1.e294dl, 1.e294, 0.)
CONVERT_VALID (317, td, df, 1.e293dl, 1.e293, 0.) 
CONVERT_VALID (318, td, df, 1.e292dl, 1.e292, 0.) 
CONVERT_VALID (319, td, df, 1.e291dl, 1.e291, 0.) 
CONVERT_VALID (320, td, df, 1.e290dl, 1.e290, 0.)

CONVERT_VALID (401, td, df, 1.e-309dl, 1.e-309, 0.) 
CONVERT_VALID (402, td, df, 1.e-308dl, 1.e-308, 0.) 
CONVERT_VALID (403, td, df, 1.e-307dl, 1.e-307, 0.) 
CONVERT_VALID (404, td, df, 1.e-306dl, 1.e-306, 0.) 
CONVERT_VALID (405, td, df, 1.e-305dl, 1.e-305, 0.) 
CONVERT_VALID (406, td, df, 1.e-304dl, 1.e-304, 0.) 
CONVERT_VALID (407, td, df, 1.e-303dl, 1.e-303, 0.) 
CONVERT_VALID (408, td, df, 1.e-302dl, 1.e-302, 0.) 
CONVERT_VALID (409, td, df, 1.e-301dl, 1.e-301, 0.) 
CONVERT_VALID (410, td, df, 1.e-300dl, 1.e-300, 0.) 
CONVERT_VALID (411, td, df, 1.e-299dl, 1.e-299, 0.) 
CONVERT_VALID (412, td, df, 1.e-298dl, 1.e-298, 0.) 
CONVERT_VALID (413, td, df, 1.e-297dl, 1.e-297, 0.) 
CONVERT_VALID (414, td, df, 1.e-296dl, 1.e-296, 0.) 
CONVERT_VALID (415, td, df, 1.e-295dl, 1.e-295, 0.) 
CONVERT_VALID (416, td, df, 1.e-294dl, 1.e-294, 0.) 
CONVERT_VALID (417, td, df, 1.e-293dl, 1.e-293, 0.) 
CONVERT_VALID (418, td, df, 1.e-292dl, 1.e-292, 0.)
CONVERT_VALID (419, td, df, 1.e-291dl, 1.e-291, 0.) 
CONVERT_VALID (420, td, df, 1.e-290dl, 1.e-290, 0.)

int
main ()
{
  convert_101 ();
  convert_102 ();
  convert_103 ();
  convert_104 ();
  convert_105 ();
  convert_106 ();
  convert_107 ();
  convert_108 ();
  convert_109 ();
  convert_110 ();
  convert_111 ();
  convert_112 ();
  convert_113 ();
  convert_114 ();
  convert_115 ();
  convert_116 ();
  convert_117 ();
  convert_118 ();
  convert_119 ();
  convert_120 ();

  convert_201 ();
  convert_202 ();
  convert_203 ();
  convert_204 ();
  convert_205 ();
  convert_206 ();
  convert_207 ();
  convert_208 ();
  convert_209 ();
  convert_210 ();
  convert_211 ();
  convert_212 ();
  convert_213 ();
  convert_214 ();
  convert_215 ();
  convert_216 ();
  convert_217 ();
  convert_218 ();
  convert_219 ();
  convert_220 ();

  convert_301 ();
  convert_302 ();
  convert_303 ();
  convert_304 ();
  convert_305 ();
  convert_306 ();
  convert_307 ();
  convert_308 ();
  convert_309 ();
  convert_310 ();
  convert_311 ();
  convert_312 ();
  convert_313 ();
  convert_314 ();
  convert_315 ();
  convert_316 ();
  convert_317 ();
  convert_318 ();
  convert_319 ();
  convert_320 ();

  convert_401 ();
  convert_402 ();
  convert_403 ();
  convert_404 ();
  convert_405 ();
  convert_406 ();
  convert_407 ();
  convert_408 ();
  convert_409 ();
  convert_410 ();
  convert_411 ();
  convert_412 ();
  convert_413 ();
  convert_414 ();
  convert_415 ();
  convert_416 ();
  convert_417 ();
  convert_418 ();
  convert_419 ();
  convert_420 ();

  FINISH
}
