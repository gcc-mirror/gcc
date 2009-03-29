/* Test boolean conversion as part of returning unsigned value does
   not lead to an ICE.  */

_Bool f(unsigned a) { return a & 1; }
