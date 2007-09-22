/* Test for valid and invalid combinations of type specifiers.
   Based off gcc.dg/test-spec-1.c */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef char type;

void _Fract *x0;  /* { dg-error "" "void _Fract" } */
char _Fract *x3;  /* { dg-error "" "char _Fract" } */
short _Fract *x6;
int _Fract *x9;   /* { dg-error "" "int _Fract" } */
long _Fract *x12;
float _Fract *x15;   /* { dg-error "" "float _Fract" } */
double _Fract *x18;   /* { dg-error "" "double _Fract" } */
signed _Fract *x21;
_Bool _Fract *x24;   /* { dg-error "" "_Bool _Fract" } */
int _Fract *x27;    /* { dg-error "" "int _Fract" } */
type _Fract *x30;   /* { dg-error "" "type _Fract" } */
char signed _Fract *x33;  /* { dg-error "" "char signed _Fract" } */
char unsigned _Fract *x36; /* { dg-error "" "char unsigned _Fract" } */
short _Fract *x39;
short signed _Fract *x42;
short unsigned _Fract *x45;
_Fract short*x48;
_Fract short signed*x51;
_Fract short unsigned*x54;
_Fract long*x57;
_Fract long signed*x60;
_Fract long unsigned*x63;
_Fract signed long*x66;
_Fract signed short*x69;
int signed _Fract *x72;  /* { dg-error "" "int signed _Fract" } */
int unsigned _Fract *x75;  /* { dg-error "" "int unsigned _Fract" } */
long int _Fract *x78;  /* { dg-error "" "long int _Fract" } */
long long _Fract *x81;
long double _Fract *x84;  /* { dg-error "" "long double _Fract" } */
long signed _Fract *x87;
long unsigned _Fract *x90;
double long _Fract *x93;  /* { dg-error "" "double long _Fract" } */
signed char _Fract *x96;   /* { dg-error "" "signed char _Fract" } */
signed short _Fract *x99;
signed int _Fract *x102;  /* { dg-error "" "signed int _Fract" } */
signed long _Fract *x105;
unsigned char _Fract *x108; /* { dg-error "" "unsigned char _Fract" } */
unsigned short _Fract *x111;
unsigned int _Fract *x114; /* { dg-error "" "unsigned int _Fract" } */
unsigned long _Fract *x117;
short int signed _Fract *x120; /* { dg-error "" "short int signed _Fract" } */
short int unsigned _Fract *x123; /* { dg-error "" "short int unsigned _Fract" } */
short signed int _Fract *x126; /* { dg-error "" "short signed int _Fract" } */
short unsigned int _Fract *x129; /* { dg-error "" "short unsigned int _Fract" } */
int short signed _Fract *x132; /* { dg-error "" "int short signed _Fract" } */
int short unsigned _Fract *x135; /* { dg-error "" "int short unsigned _Fract" } */
int long long _Fract *x138; /* { dg-error "" "int long long _Fract" } */
int long signed _Fract *x141; /* { dg-error "" "int long signed _Fract" } */
int long unsigned _Fract *x144; /* { dg-error "" "int long unsigned _Fract" } */
int signed short _Fract *x147; /* { dg-error "" "int signed short _Fract" } */
int signed long _Fract *x150; /* { dg-error "" "int signed long _Fract" } */
int unsigned short _Fract *x153; /* { dg-error "" "int unsigned short _Fract" } */
int unsigned long _Fract *x156; /* { dg-error "" "int unsigned long _Fract" } */
long int long _Fract *x159; /* { dg-error "" "long int long _Fract" } */
long int signed _Fract *x162; /* { dg-error "" "long int signed _Fract" } */
long int unsigned _Fract *x165; /* { dg-error "" "long int unsigned _Fract" } */
long long int _Fract *x168; /* { dg-error "" "long long int _Fract" } */
long long signed _Fract *x171;
long long unsigned _Fract *x175;
long signed int _Fract *x178; /* { dg-error "" "long signed int _Fract" } */
long unsigned int _Fract *x181; /* { dg-error "" "long unsigned int _Fract" } */
long unsigned long _Fract *x184;
signed short int _Fract *x187; /* { dg-error "" "signed short int _Fract" } */
signed int short _Fract *x190; /* { dg-error "" "signed int short _Fract" } */
signed int long _Fract *x192; /* { dg-error "" "signed int long _Fract" } */
signed long int _Fract *x195; /* { dg-error "" "signed long int _Fract" } */
signed long long _Fract *x198;
unsigned short int _Fract *x201; /* { dg-error "" "unsigned short int _Fract" } */
unsigned int short _Fract *x204; /* { dg-error "" "unsigned int short _Fract" } */
unsigned int long _Fract *x207; /* { dg-error "" "unsigned int long _Fract" } */
unsigned long int _Fract *x210; /* { dg-error "" "unsigned long int _Fract" } */
unsigned long long _Fract *x213;
int long long signed _Fract *x216; /* { dg-error "" "int long long signed _Fract" } */
int long long unsigned _Fract *x219; /* { dg-error "" "int long long unsigned _Fract" } */
int long signed long _Fract *x222; /* { dg-error "" "int long signed long _Fract" } */
int long unsigned long _Fract *x226; /* { dg-error "" "int long unsigned long _Fract" } */
int signed long long _Fract *x229; /* { dg-error "" "int signed long long _Fract" } */
int unsigned long long _Fract *x232; /* { dg-error "" "int unsigned long long _Fract" } */
long int long signed _Fract *x235; /* { dg-error "" "long int long signed _Fract" } */
long int long unsigned _Fract *x238; /* { dg-error "" "long int long unsigned _Fract" } */
long int signed long _Fract *x241; /* { dg-error "" "long int signed long _Fract" } */
long int unsigned long _Fract *x244; /* { dg-error "" "long int unsigned long _Fract" } */
long long int signed _Fract *x247; /* { dg-error "" "long long int signed _Fract" } */
long long int unsigned _Fract *x250; /* { dg-error "" "long long int unsigned _Fract" } */
long long signed int _Fract *x253; /* { dg-error "" "long long signed int _Fract" } */
long long unsigned int _Fract *x256; /* { dg-error "" "long long unsigned int _Fract" } */
long signed int long _Fract *x259; /* { dg-error "" "long signed int long _Fract" } */
long signed long int _Fract *x262; /* { dg-error "" "long signed long int _Fract" } */
long unsigned int long _Fract *x265; /* { dg-error "" "long unsigned int long _Fract" } */
long unsigned long int _Fract *x268; /* { dg-error "" "long unsigned long int _Fract" } */
signed long long int _Fract *x271; /* { dg-error "" "signed long long int _Fract" } */
unsigned int long long _Fract *x274; /* { dg-error "" "unsigned int long long _Fract" } */
unsigned long int long _Fract *x277; /* { dg-error "" "unsigned long int long _Fract" } */
unsigned long long int _Fract *x280; /* { dg-error "" "unsigned long long int _Fract" } */
_Complex _Fract *x283; /* { dg-error "" "_Complex _Fract" } */
_Fract _Complex *x286; /* { dg-error "" "_Fract _Complex" } */
unsigned _Fract *x289;
signed _Fract *x292;

void _Accum *k0;  /* { dg-error "" "void _Accum" } */
char _Accum *k3;  /* { dg-error "" "char _Accum" } */
short _Accum *k6;
int _Accum *k9;   /* { dg-error "" "int _Accum" } */
long _Accum *k12;
float _Accum *k15;   /* { dg-error "" "float _Accum" } */
double _Accum *k18;   /* { dg-error "" "double _Accum" } */
signed _Accum *k21;
_Bool _Accum *k24;   /* { dg-error "" "_Bool _Accum" } */
int _Accum *k27;    /* { dg-error "" "int _Accum" } */
type _Accum *k30;   /* { dg-error "" "type _Accum" } */
char signed _Accum *k33;  /* { dg-error "" "char signed _Accum" } */
char unsigned _Accum *k36; /* { dg-error "" "char unsigned _Accum" } */
short _Accum *k39;
short signed _Accum *k42;
short unsigned _Accum *k45;
_Accum short*k48;
_Accum short signed*k51;
_Accum short unsigned*k54;
_Accum long*k57;
_Accum long signed*k60;
_Accum long unsigned*k63;
_Accum signed long*k66;
_Accum signed short*k69;
int signed _Accum *k72;  /* { dg-error "" "int signed _Accum" } */
int unsigned _Accum *k75;  /* { dg-error "" "int unsigned _Accum" } */
long int _Accum *k78;  /* { dg-error "" "long int _Accum" } */
long long _Accum *k81;
long double _Accum *k84;  /* { dg-error "" "long double _Accum" } */
long signed _Accum *k87;
long unsigned _Accum *k90;
double long _Accum *k93;  /* { dg-error "" "double long _Accum" } */
signed char _Accum *k96;   /* { dg-error "" "signed char _Accum" } */
signed short _Accum *k99;
signed int _Accum *k102;  /* { dg-error "" "signed int _Accum" } */
signed long _Accum *k105;
unsigned char _Accum *k108; /* { dg-error "" "unsigned char _Accum" } */
unsigned short _Accum *k111;
unsigned int _Accum *k114; /* { dg-error "" "unsigned int _Accum" } */
unsigned long _Accum *k117;
short int signed _Accum *k120; /* { dg-error "" "short int signed _Accum" } */
short int unsigned _Accum *k123; /* { dg-error "" "short int unsigned _Accum" } */
short signed int _Accum *k126; /* { dg-error "" "short signed int _Accum" } */
short unsigned int _Accum *k129; /* { dg-error "" "short unsigned int _Accum" } */
int short signed _Accum *k132; /* { dg-error "" "int short signed _Accum" } */
int short unsigned _Accum *k135; /* { dg-error "" "int short unsigned _Accum" } */
int long long _Accum *k138; /* { dg-error "" "int long long _Accum" } */
int long signed _Accum *k141; /* { dg-error "" "int long signed _Accum" } */
int long unsigned _Accum *k144; /* { dg-error "" "int long unsigned _Accum" } */
int signed short _Accum *k147; /* { dg-error "" "int signed short _Accum" } */
int signed long _Accum *k150; /* { dg-error "" "int signed long _Accum" } */
int unsigned short _Accum *k153; /* { dg-error "" "int unsigned short _Accum" } */
int unsigned long _Accum *k156; /* { dg-error "" "int unsigned long _Accum" } */
long int long _Accum *k159; /* { dg-error "" "long int long _Accum" } */
long int signed _Accum *k162; /* { dg-error "" "long int signed _Accum" } */
long int unsigned _Accum *k165; /* { dg-error "" "long int unsigned _Accum" } */
long long int _Accum *k168; /* { dg-error "" "long long int _Accum" } */
long long signed _Accum *k171;
long long unsigned _Accum *k175;
long signed int _Accum *k178; /* { dg-error "" "long signed int _Accum" } */
long unsigned int _Accum *k181; /* { dg-error "" "long unsigned int _Accum" } */
long unsigned long _Accum *k184;
signed short int _Accum *k187; /* { dg-error "" "signed short int _Accum" } */
signed int short _Accum *k190; /* { dg-error "" "signed int short _Accum" } */
signed int long _Accum *k192; /* { dg-error "" "signed int long _Accum" } */
signed long int _Accum *k195; /* { dg-error "" "signed long int _Accum" } */
signed long long _Accum *k198;
unsigned short int _Accum *k201; /* { dg-error "" "unsigned short int _Accum" } */
unsigned int short _Accum *k204; /* { dg-error "" "unsigned int short _Accum" } */
unsigned int long _Accum *k207; /* { dg-error "" "unsigned int long _Accum" } */
unsigned long int _Accum *k210; /* { dg-error "" "unsigned long int _Accum" } */
unsigned long long _Accum *k213;
int long long signed _Accum *k216; /* { dg-error "" "int long long signed _Accum" } */
int long long unsigned _Accum *k219; /* { dg-error "" "int long long unsigned _Accum" } */
int long signed long _Accum *k222; /* { dg-error "" "int long signed long _Accum" } */
int long unsigned long _Accum *k226; /* { dg-error "" "int long unsigned long _Accum" } */
int signed long long _Accum *k229; /* { dg-error "" "int signed long long _Accum" } */
int unsigned long long _Accum *k232; /* { dg-error "" "int unsigned long long _Accum" } */
long int long signed _Accum *k235; /* { dg-error "" "long int long signed _Accum" } */
long int long unsigned _Accum *k238; /* { dg-error "" "long int long unsigned _Accum" } */
long int signed long _Accum *k241; /* { dg-error "" "long int signed long _Accum" } */
long int unsigned long _Accum *k244; /* { dg-error "" "long int unsigned long _Accum" } */
long long int signed _Accum *k247; /* { dg-error "" "long long int signed _Accum" } */
long long int unsigned _Accum *k250; /* { dg-error "" "long long int unsigned _Accum" } */
long long signed int _Accum *k253; /* { dg-error "" "long long signed int _Accum" } */
long long unsigned int _Accum *k256; /* { dg-error "" "long long unsigned int _Accum" } */
long signed int long _Accum *k259; /* { dg-error "" "long signed int long _Accum" } */
long signed long int _Accum *k262; /* { dg-error "" "long signed long int _Accum" } */
long unsigned int long _Accum *k265; /* { dg-error "" "long unsigned int long _Accum" } */
long unsigned long int _Accum *k268; /* { dg-error "" "long unsigned long int _Accum" } */
signed long long int _Accum *k271; /* { dg-error "" "signed long long int _Accum" } */
unsigned int long long _Accum *k274; /* { dg-error "" "unsigned int long long _Accum" } */
unsigned long int long _Accum *k277; /* { dg-error "" "unsigned long int long _Accum" } */
unsigned long long int _Accum *k280; /* { dg-error "" "unsigned long long int _Accum" } */
_Complex _Accum *k283; /* { dg-error "" "_Complex _Accum" } */
_Accum _Complex *k286; /* { dg-error "" "_Accum _Complex" } */
unsigned _Accum *k289;
signed _Accum *k292;

void _Sat *s0;  /* { dg-error "" "void _Sat" } */
char _Sat *s3;  /* { dg-error "" "char _Sat" } */
short _Sat *s6; /* { dg-error "" "short _Sat" } */
int _Sat *s9;   /* { dg-error "" "int _Sat" } */
long _Sat *s12;   /* { dg-error "" "long _Sat" } */
float _Sat *s15;   /* { dg-error "" "float _Sat" } */
double _Sat *s18;   /* { dg-error "" "double _Sat" } */
signed _Sat *s21;   /* { dg-error "" "signed _Sat" } */
_Bool _Sat *s24;   /* { dg-error "" "_Bool _Sat" } */
int _Sat *s27;   /* { dg-error "" "int _Sat" } */
type _Sat *s30;   /* { dg-error "" "type _Sat" } */
char signed _Sat *s33;  /* { dg-error "" "char signed _Sat" } */
char unsigned _Sat *s36; /* { dg-error "" "char unsigned _Sat" } */
short _Sat *s39;  /* { dg-error "" "short _Sat" } */
short signed _Sat *s42;  /* { dg-error "" "short signed _Sat" } */
short unsigned _Sat *s45;  /* { dg-error "" "short unsigned _Sat" } */
_Sat short*s48;  /* { dg-error "" "_Sat short" } */
_Sat short signed*s51;  /* { dg-error "" "_Sat short signed" } */
_Sat short unsigned*s54;  /* { dg-error "" "_Sat short unsigned" } */
_Sat long*s57;  /* { dg-error "" "_Sat long" } */
_Sat long signed*s60;  /* { dg-error "" "_Sat long signed" } */
_Sat long unsigned*s63;  /* { dg-error "" "_Sat long unsigned" } */
_Sat signed long*s66;  /* { dg-error "" "_Sat signed long" } */
_Sat signed short*s69;  /* { dg-error "" "_Sat signed short" } */
int signed _Sat *s72;  /* { dg-error "" "int signed _Sat" } */
int unsigned _Sat *s75;  /* { dg-error "" "int unsigned _Sat" } */
long int _Sat *s78;  /* { dg-error "" "long int _Sat" } */
long long _Sat *s81;  /* { dg-error "" "long long _Sat" } */
long double _Sat *s84;  /* { dg-error "" "long double _Sat" } */
long signed _Sat *s87;  /* { dg-error "" "long signed _Sat" } */
long unsigned _Sat *s90;  /* { dg-error "" "long unsigned _Sat" } */
double long _Sat *s93;  /* { dg-error "" "double long _Sat" } */
signed char _Sat *s96;   /* { dg-error "" "signed char _Sat" } */
signed short _Sat *s99;   /* { dg-error "" "signed short _Sat" } */
signed int _Sat *s102;  /* { dg-error "" "signed int _Sat" } */
signed long _Sat *s105; /* { dg-error "" "signed long _Sat" } */
unsigned char _Sat *s108; /* { dg-error "" "unsigned char _Sat" } */
unsigned short _Sat *s111; /* { dg-error "" "unsigned short _Sat" } */
unsigned int _Sat *s114; /* { dg-error "" "unsigned int _Sat" } */
unsigned long _Sat *s117; /* { dg-error "" "unsigned long _Sat" } */
short int signed _Sat *s120; /* { dg-error "" "short int signed _Sat" } */
short int unsigned _Sat *s123; /* { dg-error "" "short int unsigned _Sat" } */
short signed int _Sat *s126; /* { dg-error "" "short signed int _Sat" } */
short unsigned int _Sat *s129; /* { dg-error "" "short unsigned int _Sat" } */
int short signed _Sat *s132; /* { dg-error "" "int short signed _Sat" } */
int short unsigned _Sat *s135; /* { dg-error "" "int short unsigned _Sat" } */
int long long _Sat *s138; /* { dg-error "" "int long long _Sat" } */
int long signed _Sat *s141; /* { dg-error "" "int long signed _Sat" } */
int long unsigned _Sat *s144; /* { dg-error "" "int long unsigned _Sat" } */
int signed short _Sat *s147; /* { dg-error "" "int signed short _Sat" } */
int signed long _Sat *s150; /* { dg-error "" "int signed long _Sat" } */
int unsigned short _Sat *s153; /* { dg-error "" "int unsigned short _Sat" } */
int unsigned long _Sat *s156; /* { dg-error "" "int unsigned long _Sat" } */
long int long _Sat *s159; /* { dg-error "" "long int long _Sat" } */
long int signed _Sat *s162; /* { dg-error "" "long int signed _Sat" } */
long int unsigned _Sat *s165; /* { dg-error "" "long int unsigned _Sat" } */
long long int _Sat *s168; /* { dg-error "" "long long int _Sat" } */
long long signed _Sat *s171; /* { dg-error "" "long long signed _Sat" } */
long long unsigned _Sat *s175; /* { dg-error "" "long long unsigned _Sat" } */
long signed int _Sat *s178; /* { dg-error "" "long signed int _Sat" } */
long unsigned int _Sat *s181; /* { dg-error "" "long unsigned int _Sat" } */
long unsigned long _Sat *s184; /* { dg-error "" "long unsigned long _Sat" } */
signed short int _Sat *s187; /* { dg-error "" "signed short int _Sat" } */
signed int short _Sat *s190; /* { dg-error "" "signed int short _Sat" } */
signed int long _Sat *s192; /* { dg-error "" "signed int long _Sat" } */
signed long int _Sat *s195; /* { dg-error "" "signed long int _Sat" } */
signed long long _Sat *s198; /* { dg-error "" "signed long long _Sat" } */
unsigned short int _Sat *s201; /* { dg-error "" "unsigned short int _Sat" } */
unsigned int short _Sat *s204; /* { dg-error "" "unsigned int short _Sat" } */
unsigned int long _Sat *s207; /* { dg-error "" "unsigned int long _Sat" } */
unsigned long int _Sat *s210; /* { dg-error "" "unsigned long int _Sat" } */
unsigned long long _Sat *s213; /* { dg-error "" "unsigned long long _Sat" } */
int long long signed _Sat *s216; /* { dg-error "" "int long long signed _Sat" } */
int long long unsigned _Sat *s219; /* { dg-error "" "int long long unsigned _Sat" } */
int long signed long _Sat *s222; /* { dg-error "" "int long signed long _Sat" } */
int long unsigned long _Sat *s226; /* { dg-error "" "int long unsigned long _Sat" } */
int signed long long _Sat *s229; /* { dg-error "" "int signed long long _Sat" } */
int unsigned long long _Sat *s232; /* { dg-error "" "int unsigned long long _Sat" } */
long int long signed _Sat *s235; /* { dg-error "" "long int long signed _Sat" } */
long int long unsigned _Sat *s238; /* { dg-error "" "long int long unsigned _Sat" } */
long int signed long _Sat *s241; /* { dg-error "" "long int signed long _Sat" } */
long int unsigned long _Sat *s244; /* { dg-error "" "long int unsigned long _Sat" } */
long long int signed _Sat *s247; /* { dg-error "" "long long int signed _Sat" } */
long long int unsigned _Sat *s250; /* { dg-error "" "long long int unsigned _Sat" } */
long long signed int _Sat *s253; /* { dg-error "" "long long signed int _Sat" } */
long long unsigned int _Sat *s256; /* { dg-error "" "long long unsigned int _Sat" } */
long signed int long _Sat *s259; /* { dg-error "" "long signed int long _Sat" } */
long signed long int _Sat *s262; /* { dg-error "" "long signed long int _Sat" } */
long unsigned int long _Sat *s265; /* { dg-error "" "long unsigned int long _Sat" } */
long unsigned long int _Sat *s268; /* { dg-error "" "long unsigned long int _Sat" } */
signed long long int _Sat *s271; /* { dg-error "" "signed long long int _Sat" } */
unsigned int long long _Sat *s274; /* { dg-error "" "unsigned int long long _Sat" } */
unsigned long int long _Sat *s277; /* { dg-error "" "unsigned long int long _Sat" } */
unsigned long long int _Sat *s280; /* { dg-error "" "unsigned long long int _Sat" } */
_Complex _Sat *s283; /* { dg-error "" "_Complex _Sat" } */
_Sat _Complex *s286; /* { dg-error "" "_Sat _Complex" } */
unsigned _Sat *s289; /* { dg-error "" "unsigned _Sat" } */
signed _Sat *s292; /* { dg-error "" "signed _Sat" } */
