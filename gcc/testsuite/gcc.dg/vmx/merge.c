#include "harness.h"

static void test()
{
  /* Input vectors.  */
  vector unsigned char vuca = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector unsigned char vucb
    = {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
  vector signed char vsca
    = {-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed char vscb = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector unsigned short vusa = {0,1,2,3,4,5,6,7};
  vector unsigned short vusb = {8,9,10,11,12,13,14,15};
  vector signed short vssa = {-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed short vssb = {0,1,2,3,4,5,6,7};
  vector unsigned int vuia = {0,1,2,3};
  vector unsigned int vuib = {4,5,6,7};
  vector signed int vsia = {-4,-3,-2,-1};
  vector signed int vsib = {0,1,2,3};
  vector float vfa = {-4.0,-3.0,-2.0,-1.0};
  vector float vfb = {0.0,1.0,2.0,3.0};

  /* Result vectors.  */
  vector unsigned char vuch, vucl;
  vector signed char vsch, vscl;
  vector unsigned short vush, vusl;
  vector signed short vssh, vssl;
  vector unsigned int vuih, vuil;
  vector signed int vsih, vsil;
  vector float vfh, vfl;

  /* Expected result vectors.  */
  vector unsigned char vucrh = {0,16,1,17,2,18,3,19,4,20,5,21,6,22,7,23};
  vector unsigned char vucrl = {8,24,9,25,10,26,11,27,12,28,13,29,14,30,15,31};
  vector signed char vscrh = {-16,0,-15,1,-14,2,-13,3,-12,4,-11,5,-10,6,-9,7};
  vector signed char vscrl = {-8,8,-7,9,-6,10,-5,11,-4,12,-3,13,-2,14,-1,15};
  vector unsigned short vusrh = {0,8,1,9,2,10,3,11};
  vector unsigned short vusrl = {4,12,5,13,6,14,7,15};
  vector signed short vssrh = {-8,0,-7,1,-6,2,-5,3};
  vector signed short vssrl = {-4,4,-3,5,-2,6,-1,7};
  vector unsigned int vuirh = {0,4,1,5};
  vector unsigned int vuirl = {2,6,3,7};
  vector signed int vsirh = {-4,0,-3,1};
  vector signed int vsirl = {-2,2,-1,3};
  vector float vfrh = {-4.0,0.0,-3.0,1.0};
  vector float vfrl = {-2.0,2.0,-1.0,3.0};

  vuch = vec_mergeh (vuca, vucb);
  vucl = vec_mergel (vuca, vucb);
  vsch = vec_mergeh (vsca, vscb);
  vscl = vec_mergel (vsca, vscb);
  vush = vec_mergeh (vusa, vusb);
  vusl = vec_mergel (vusa, vusb);
  vssh = vec_mergeh (vssa, vssb);
  vssl = vec_mergel (vssa, vssb);
  vuih = vec_mergeh (vuia, vuib);
  vuil = vec_mergel (vuia, vuib);
  vsih = vec_mergeh (vsia, vsib);
  vsil = vec_mergel (vsia, vsib);
  vfh  = vec_mergeh (vfa,  vfb );
  vfl  = vec_mergel (vfa,  vfb );

  check (vec_all_eq (vuch, vucrh), "vuch");
  check (vec_all_eq (vucl, vucrl), "vucl");
  check (vec_all_eq (vsch, vscrh), "vsch");
  check (vec_all_eq (vscl, vscrl), "vscl");
  check (vec_all_eq (vush, vusrh), "vush");
  check (vec_all_eq (vusl, vusrl), "vusl");
  check (vec_all_eq (vssh, vssrh), "vssh");
  check (vec_all_eq (vssl, vssrl), "vssl");
  check (vec_all_eq (vuih, vuirh), "vuih");
  check (vec_all_eq (vuil, vuirl), "vuil");
  check (vec_all_eq (vsih, vsirh), "vsih");
  check (vec_all_eq (vsil, vsirl), "vsil");
  check (vec_all_eq (vfh,  vfrh),  "vfh");
  check (vec_all_eq (vfl,  vfrl),  "vfl");
}
