/* Undo the USG definition in xm-rs6000.h, Darwin is a BSD flavor.  */

#undef USG

/* Override the usual setting, since Apple's GCC has lame bugs and
   can't handle the initializers.  Someday the bugs will be fixed and
   we can get rid of this silliness.  */

#define HAVE_DESIGNATED_INITIALIZERS 0
