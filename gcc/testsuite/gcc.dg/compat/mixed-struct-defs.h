/* Type definitions that are used by multiple tests.  */

typedef struct { char c; double d; } Scd;
typedef struct { char c; double d; char b; } Scdc;
typedef struct { double d; } Sd;
typedef struct { double d; int i; } Sdi;
typedef struct { char c; Sd sd; } Scsds;
typedef struct { char c; Sd sd; char b; } Scsdsc;
typedef struct { char c; Sdi sdi; } Scsdis;
typedef struct { char c; Sdi sdi; char b; } Scsdisc;
typedef struct { Sd sd; } Ssds;
typedef struct { Sd sd; char c; } Ssdsc;
typedef struct { char c; Ssds ssds; } Scssdss;
typedef struct { char c; Ssds ssds; char b; } Scssdssc;

typedef struct { float f; int i; } Sfi;
typedef struct { float f; int i1; int i2; } Sfii;
typedef struct { float f; int i; Sfi fi; } Sfifi;
typedef struct { float f; int i1; int i2; Sfii fii; } Sfiifii;
