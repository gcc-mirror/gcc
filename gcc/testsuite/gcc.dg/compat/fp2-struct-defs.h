/* Type definitions that are used by multiple tests.  */

typedef struct { float f; double d; } Sfd;
typedef struct { float f; long double l; } Sfl;
typedef struct { double d; float f; } Sdf;
typedef struct { double d; long double l; } Sdl;
typedef struct { long double l; float f; } Slf;
typedef struct { long double l; double d; } Sld;

typedef struct { float f; double d; long double l; } Sfdl;
typedef struct { float f; long double l; double d; } Sfld;
typedef struct { double d; float f; long double l; } Sdfl;
typedef struct { double d; long double l; float f; } Sdlf;
typedef struct { long double l; float f; double d; } Slfd;
typedef struct { long double l; double d; float f; } Sldf;
