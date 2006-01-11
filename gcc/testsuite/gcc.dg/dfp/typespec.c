/* Test for valid and invalid combinations of type specifiers.
   Based off gcc.dg/test-spec-1.c */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef char type;
void _Decimal32 *x0;  /* { dg-error "error" "void _Decimal32" } */
void _Decimal64 *x1;  /* { dg-error "error" "void _Decimal64" } */
void _Decimal128 *x2; /* { dg-error "error" "void _Decimal128" } */
char _Decimal32 *x3;  /* { dg-error "error" "char _Decimal32" } */
char _Decimal64 *x4;  /* { dg-error "error" "char _Decimal64" } */
char _Decimal128 *x5;  /* { dg-error "error" "char _Decimal128" } */
short _Decimal32 *x6; /* { dg-error "error" "short _Decimal32" } */
short _Decimal64 *x7; /* { dg-error "error" "short _Decimal64" } */
short _Decimal128 *x8; /* { dg-error "error" "short _Decimal128" } */
int _Decimal32 *x9;   /* { dg-error "error" "int _Decimal32" } */
int _Decimal64 *x10;  /* { dg-error "error" "int _Decimal64" } */
int _Decimal128 *x11; /* { dg-error "error" "int _Decimal128" } */
long _Decimal32 *x12;   /* { dg-error "error" "long _Decimal32" } */
long _Decimal64 *x13;  /* { dg-error "error" "long _Decimal64" } */
long _Decimal128 *x14; /* { dg-error "error" "long _Decimal128" } */
float _Decimal32 *x15;   /* { dg-error "error" "float _Decimal32" } */
float _Decimal64 *x16;  /* { dg-error "error" "float _Decimal64" } */
float _Decimal128 *x17; /* { dg-error "error" "float _Decimal128" } */
double _Decimal32 *x18;   /* { dg-error "error" "double _Decimal32" } */
double _Decimal64 *x19;  /* { dg-error "error" "double _Decimal64" } */
double _Decimal128 *x20; /* { dg-error "error" "double _Decimal128" } */
signed _Decimal32 *x21;   /* { dg-error "error" "signed _Decimal32" } */
signed _Decimal64 *x22;  /* { dg-error "error" "signed _Decimal64" } */
signed _Decimal128 *x23; /* { dg-error "error" "signed _Decimal128" } */
_Bool _Decimal32 *x24;   /* { dg-error "error" "_Bool _Decimal32" } */
_Bool _Decimal64 *x25;  /* { dg-error "error" "_Bool _Decimal64" } */
_Bool _Decimal128 *x26; /* { dg-error "error" "_Bool _Decimal128" } */
int _Decimal32 *x27;   /* { dg-error "error" "int _Decimal32" } */
int _Decimal64 *x28;  /* { dg-error "error" "int _Decimal64" } */
int _Decimal128 *x29; /* { dg-error "error" "int _Decimal128" } */
type _Decimal32 *x30;   /* { dg-error "error" "type _Decimal32" } */
type _Decimal64 *x31;  /* { dg-error "error" "type _Decimal64" } */
type _Decimal128 *x32; /* { dg-error "error" "type _Decimal128" } */
char signed _Decimal32 *x33;  /* { dg-error "error" "char signed _Decimal32" } */
char signed _Decimal64 *x34;  /* { dg-error "error" "char signed _Decimal64" } */
char signed _Decimal128 *x35; /* { dg-error "error" "char signed _Decimal128" } */
char unsigned _Decimal32 *x36; /* { dg-error "error" "char unsigned _Decimal32" } */
char unsigned _Decimal64 *x37; /* { dg-error "error" "char unsigned _Decimal64" } */
char unsigned _Decimal128 *x38; /* { dg-error "error" "char unsigned _Decimal128" } */
short _Decimal32 *x39;  /* { dg-error "error" "short _Decimal32" } */
short _Decimal64 *x40;  /* { dg-error "error" "short _Decimal64" } */
short _Decimal128 *x41; /* { dg-error "error" "short _Decimal128" } */
short signed _Decimal32 *x42;  /* { dg-error "error" "short signed _Decimal32" } */
short signed _Decimal64 *x43;  /* { dg-error "error" "short signed _Decimal64" } */
short signed _Decimal128 *x44; /* { dg-error "error" "short signed _Decimal128" } */
short unsigned _Decimal32 *x45;  /* { dg-error "error" "short unsigned _Decimal32" } */
short unsigned _Decimal64 *x46;  /* { dg-error "error" "short unsigned _Decimal64" } */
short unsigned _Decimal128 *x47; /* { dg-error "error" "short unsigned _Decimal128" } */
_Decimal32 short*x48;  /* { dg-error "error" "_Decimal32 short" } */
_Decimal64 short*x49;  /* { dg-error "error" "_Decimal64 short" } */
_Decimal128 short*x50; /* { dg-error "error" "_Decimal128 short" } */
_Decimal32 short signed*x51;  /* { dg-error "error" "_Decimal32 short signed" } */
_Decimal64 short signed*x52;  /* { dg-error "error" "_Decimal64 short signed" } */
_Decimal128 short signed*x53; /* { dg-error "error" "_Decimal128 short signed" } */
_Decimal32 short unsigned*x54;  /* { dg-error "error" "_Decimal32 short unsigned" } */
_Decimal64 short unsigned*x55;  /* { dg-error "error" "_Decimal64 short unsigned" } */
_Decimal128 short unsigned*x56; /* { dg-error "error" "_Decimal128 short unsigned" } */
_Decimal32 long*x57;  /* { dg-error "error" "_Decimal32 long" } */
_Decimal64 long*x58;  /* { dg-error "error" "_Decimal64 long" } */
_Decimal128 long*x59; /* { dg-error "error" "_Decimal128 long" } */
_Decimal32 long signed*x60;  /* { dg-error "error" "_Decimal32 long signed" } */
_Decimal64 long signed*x61;  /* { dg-error "error" "_Decimal64 long signed" } */
_Decimal128 long signed*x62; /* { dg-error "error" "_Decimal128 long signed" } */
_Decimal32 long unsigned*x63;  /* { dg-error "error" "_Decimal32 long unsigned" } */
_Decimal64 long unsigned*x64;  /* { dg-error "error" "_Decimal64 long unsigned" } */
_Decimal128 long unsigned*x65; /* { dg-error "error" "_Decimal128 long unsigned" } */
_Decimal32 signed long*x66;  /* { dg-error "error" "_Decimal32 signed long" } */
_Decimal64 signed long*x67;  /* { dg-error "error" "_Decimal64 signed long" } */
_Decimal128 signed long*x68; /* { dg-error "error" "_Decimal128 signed long" } */
_Decimal32 signed short*x69;  /* { dg-error "error" "_Decimal32 signed short" } */
_Decimal64 signed short*x70;  /* { dg-error "error" "_Decimal64 signed short" } */
_Decimal128 signed short*x71; /* { dg-error "error" "_Decimal128 signed short" } */
int signed _Decimal32 *x72;  /* { dg-error "error" "int signed _Decimal32" } */
int signed _Decimal64 *x73;  /* { dg-error "error" "int signed _Decimal64" } */
int signed _Decimal128 *x74;  /* { dg-error "error" "int signed _Decimal128" } */
int unsigned _Decimal32 *x75;  /* { dg-error "error" "int unsigned _Decimal32" } */
int unsigned _Decimal64 *x76;  /* { dg-error "error" "int unsigned _Decimal64" } */
int unsigned _Decimal128 *x77; /* { dg-error "error" "int unsigned _Decimal128" } */
long int _Decimal32 *x78;  /* { dg-error "error" "long int _Decimal32" } */
long int _Decimal64 *x79;  /* { dg-error "error" "long int _Decimal64" } */
long int _Decimal128 *x80; /* { dg-error "error" "long int _Decimal128" } */
long long _Decimal32 *x81;  /* { dg-error "error" "long long _Decimal32" } */
long long _Decimal64 *x82;  /* { dg-error "error" "long long _Decimal64" } */
long long _Decimal128 *x83; /* { dg-error "error" "long long _Decimal128" } */
long double _Decimal32 *x84;  /* { dg-error "error" "long double _Decimal32" } */
long double _Decimal64 *x85;  /* { dg-error "error" "long double _Decimal64" } */
long double _Decimal128 *x86; /* { dg-error "error" "long double _Decimal128" } */
long signed _Decimal32 *x87;  /* { dg-error "error" "long signed _Decimal32" } */
long signed _Decimal64 *x88;  /* { dg-error "error" "long signed _Decimal64" } */
long signed _Decimal128 *x89; /* { dg-error "error" "long signed _Decimal128" } */
long unsigned _Decimal32 *x90;  /* { dg-error "error" "long unsigned _Decimal32" } */
long unsigned _Decimal64 *x91;  /* { dg-error "error" "long unsigned _Decimal64" } */
long unsigned _Decimal128 *x92; /* { dg-error "error" "long unsigned _Decimal128" } */
double long _Decimal32 *x93;  /* { dg-error "error" "double long _Decimal32" } */
double long _Decimal64 *x94;  /* { dg-error "error" "double long _Decimal64" } */
double long _Decimal128 *x95; /* { dg-error "error" "double long _Decimal128" } */
signed char _Decimal32 *x96;   /* { dg-error "error" "signed char _Decimal32" } */
signed char _Decimal64 *x97;  /* { dg-error "error" "signed char _Decimal64" } */
signed char _Decimal128 *x98; /* { dg-error "error" "signed char _Decimal128" } */
signed short _Decimal32 *x99;   /* { dg-error "error" "signed short _Decimal32" } */
signed short _Decimal64 *x100;  /* { dg-error "error" "signed short _Decimal64" } */
signed short _Decimal128 *x101; /* { dg-error "error" "signed short _Decimal128" } */
signed int _Decimal32 *x102;  /* { dg-error "error" "signed int _Decimal32" } */
signed int _Decimal64 *x103;  /* { dg-error "error" "signed int _Decimal64" } */
signed int _Decimal128 *x104; /* { dg-error "error" "signed int _Decimal128" } */
signed long _Decimal32 *105x; /* { dg-error "error" "signed long _Decimal32" } */
signed long _Decimal64 *x107; /* { dg-error "error" "signed long _Decimal64" } \*/
signed long _Decimal128 *x107; /* { dg-error "error" "signed long _Decimal128" } \*/
unsigned char _Decimal32 *x108; /* { dg-error "error" "unsigned char _Decimal32" } */
unsigned char _Decimal64 *x109; /* { dg-error "error" "unsigned char _Decimal64" } */
unsigned char _Decimal128 *x110; /* { dg-error "error" "unsigned char _Decimal128" } */
unsigned short _Decimal32 *x111; /* { dg-error "error" "unsigned short _Decimal32" } */
unsigned short _Decimal64 *x112; /* { dg-error "error" "unsigned short _Decimal64" } */
unsigned short _Decimal128 *x113; /* { dg-error "error" "unsigned short _Decimal128" } */
unsigned int _Decimal32 *x114; /* { dg-error "error" "unsigned int _Decimal32" } */
unsigned int _Decimal64 *x115; /* { dg-error "error" "unsigned int _Decimal64" } */
unsigned int _Decimal128 *x116; /* { dg-error "error" "unsigned int _Decimal128" } */
unsigned long _Decimal32 *x117; /* { dg-error "error" "unsigned long _Decimal32" } */
unsigned long _Decimal64 *x118; /* { dg-error "error" "unsigned long _Decimal64" } */
unsigned long _Decimal128 *x119; /* { dg-error "error" "unsigned long _Decimal128" } */
short int signed _Decimal32 *x120; /* { dg-error "error" "short int signed _Decimal32" } */
short int signed _Decimal64 *x121; /* { dg-error "error" "short int signed _Decimal64" } */
short int signed _Decimal128 *x122; /* { dg-error "error" "short int signed _Decimal128" } */
short int unsigned _Decimal32 *x123; /* { dg-error "error" "short int unsigned _Decimal32" } */
short int unsigned _Decimal64 *x124; /* { dg-error "error" "short int unsigned _Decimal64" } */
short int unsigned _Decimal128 *x125; /* { dg-error "error" "short int unsigned _Decimal128" } */
short signed int _Decimal32 *x126; /* { dg-error "error" "short signed int _Decimal32" } */
short signed int _Decimal64 *x127; /* { dg-error "error" "short signed int _Decimal64" } */
short signed int _Decimal128 *x128; /* { dg-error "error" "short signed int _Decimal128" } */
short unsigned int _Decimal32 *x129; /* { dg-error "error" "short unsigned int _Decimal32" } */
short unsigned int _Decimal64 *x130; /* { dg-error "error" "short unsigned int _Decimal64" } */
short unsigned int _Decimal128 *x131; /* { dg-error "error" "short unsigned int _Decimal128" } */
int short signed _Decimal32 *x132; /* { dg-error "error" "int short signed _Decimal32" } */
int short signed _Decimal64 *x133; /* { dg-error "error" "int short signed _Decimal64" } */
int short signed _Decimal128 *x134; /* { dg-error "error" "int short signed _Decimal128" } */
int short unsigned _Decimal32 *x135; /* { dg-error "error" "int short unsigned _Decimal32" } */
int short unsigned _Decimal64 *x136; /* { dg-error "error" "int short unsigned _Decimal64" } */
int short unsigned _Decimal128 *x137; /* { dg-error "error" "int short unsigned _Decimal128" } */
int long long _Decimal32 *x138; /* { dg-error "error" "int long long _Decimal32" } */
int long long _Decimal64 *x139; /* { dg-error "error" "int long long _Decimal64" } */
int long long _Decimal128 *x140; /* { dg-error "error" "int long long _Decimal128" } */
int long signed _Decimal32 *x141; /* { dg-error "error" "int long signed _Decimal32" } */
int long signed _Decimal64 *x142; /* { dg-error "error" "int long signed _Decimal64" } */
int long signed _Decimal128 *x143; /* { dg-error "error" "int long signed _Decimal128" } */
int long unsigned _Decimal32 *x144; /* { dg-error "error" "int long unsigned _Decimal32" } */
int long unsigned _Decimal64 *x145; /* { dg-error "error" "int long unsigned _Decimal64" } */
int long unsigned _Decimal128 *x146; /* { dg-error "error" "int long unsigned _Decimal128" } */
int signed short _Decimal32 *x147; /* { dg-error "error" "int signed short _Decimal32" } */
int signed short _Decimal64 *x148; /* { dg-error "error" "int signed short _Decimal64" } */
int signed short _Decimal128 *x149; /* { dg-error "error" "int signed short _Decimal128" } */
int signed long _Decimal32 *x150; /* { dg-error "error" "int signed long _Decimal32" } */
int signed long _Decimal64 *x151; /* { dg-error "error" "int signed long _Decimal64" } */
int signed long _Decimal128 *x152; /* { dg-error "error" "int signed long _Decimal128" } */
int unsigned short _Decimal32 *x153; /* { dg-error "error" "int unsigned short _Decimal32" } */
int unsigned short _Decimal64 *x154; /* { dg-error "error" "int unsigned short _Decimal64" } */
int unsigned short _Decimal128 *x155; /* { dg-error "error" "int unsigned short _Decimal128" } */
int unsigned long _Decimal32 *x156; /* { dg-error "error" "int unsigned long _Decimal32" } */
int unsigned long _Decimal64 *x157; /* { dg-error "error" "int unsigned long _Decimal64" } */
int unsigned long _Decimal128 *x158; /* { dg-error "error" "int unsigned long _Decimal128" } */
long int long _Decimal32 *x159; /* { dg-error "error" "long int long _Decimal32" } */
long int long _Decimal64 *x160; /* { dg-error "error" "long int long _Decimal64" } */
long int long _Decimal128 *x161; /* { dg-error "error" "long int long _Decimal128" } */
long int signed _Decimal32 *x162; /* { dg-error "error" "long int signed _Decimal32" } */
long int signed _Decimal64 *x163; /* { dg-error "error" "long int signed _Decimal64" } */
long int signed _Decimal128 *x164; /* { dg-error "error" "long int signed _Decimal128" } */
long int unsigned _Decimal32 *x165; /* { dg-error "error" "long int unsigned _Decimal32" } */
long int unsigned _Decimal64 *x166; /* { dg-error "error" "long int unsigned _Decimal64" } */
long int unsigned _Decimal128 *x167; /* { dg-error "error" "long int unsigned _Decimal128" } */
long long int _Decimal32 *x168; /* { dg-error "error" "long long int _Decimal32" } */
long long int _Decimal64 *x169; /* { dg-error "error" "long long int _Decimal64" } */
long long int _Decimal128 *x170; /* { dg-error "error" "long long int _Decimal128" } */
long long signed _Decimal32 *x171; /* { dg-error "error" "long long signed _Decimal32" } */
long long signed _Decimal64 *x172; /* { dg-error "error" "long long signed _Decimal64" } */
long long signed _Decimal128 *x172; /* { dg-error "error" "long long signed _Decimal128" } */
long long unsigned _Decimal32 *x175; /* { dg-error "error" "long long unsigned _Decimal32" } */
long long unsigned _Decimal64 *x176; /* { dg-error "error" "long long unsigned _Decimal64" } */
long long unsigned _Decimal128 *x177; /* { dg-error "error" "long long unsigned _Decimal128" } */
long signed int _Decimal32 *x178; /* { dg-error "error" "long signed int _Decimal32" } */
long signed int _Decimal64 *x179; /* { dg-error "error" "long signed int _Decimal64" } */
long signed int _Decimal128 *x180; /* { dg-error "error" "long signed int _Decimal128" } */
long unsigned int _Decimal32 *x181; /* { dg-error "error" "long unsigned int _Decimal32" } */
long unsigned int _Decimal64 *x182; /* { dg-error "error" "long unsigned int _Decimal64" } */
long unsigned int _Decimal128 *x183; /* { dg-error "error" "long unsigned int _Decimal128" } */
long unsigned long _Decimal32 *x184; /* { dg-error "error" "long unsigned long _Decimal32" } */
long unsigned long _Decimal64 *x185; /* { dg-error "error" "long unsigned long _Decimal64" } */
long unsigned long _Decimal128 *x186; /* { dg-error "error" "long unsigned long _Decimal128" } */
signed short int _Decimal32 *x187; /* { dg-error "error" "signed short int _Decimal32" } */
signed short int _Decimal64 *x188; /* { dg-error "error" "signed short int _Decimal64" } */
signed short int _Decimal128 *x189; /* { dg-error "error" "signed short int _Decimal128" } */
signed int short _Decimal32 *x190; /* { dg-error "error" "signed int short _Decimal32" } */
signed int short _Decimal64 *x191; /* { dg-error "error" "signed int short _Decimal64" } */
signed int short _Decimal128 *x191; /* { dg-error "error" "signed int short _Decimal128" } */
signed int long _Decimal32 *x192; /* { dg-error "error" "signed int long _Decimal32" } */
signed int long _Decimal64 *x193; /* { dg-error "error" "signed int long _Decimal64" } */
signed int long _Decimal128 *x194; /* { dg-error "error" "signed int long _Decimal128" } */
signed long int _Decimal32 *x195; /* { dg-error "error" "signed long int _Decimal32" } */
signed long int _Decimal64 *x196; /* { dg-error "error" "signed long int _Decimal64" } */
signed long int _Decimal128 *x197; /* { dg-error "error" "signed long int _Decimal128" } */
signed long long _Decimal32 *x198; /* { dg-error "error" "signed long long _Decimal32" } */
signed long long _Decimal64 *x199; /* { dg-error "error" "signed long long _Decimal64" } */
signed long long _Decimal128 *x200; /* { dg-error "error" "signed long long _Decimal128" } */
unsigned short int _Decimal32 *x201; /* { dg-error "error" "unsigned short int _Decimal32" } */
unsigned short int _Decimal64 *x202; /* { dg-error "error" "unsigned short int _Decimal64" } */
unsigned short int _Decimal128 *x203; /* { dg-error "error" "unsigned short int _Decimal128" } */
unsigned int short _Decimal32 *x204; /* { dg-error "error" "unsigned int short _Decimal32" } */
unsigned int short _Decimal64 *x205; /* { dg-error "error" "unsigned int short _Decimal64" } */
unsigned int short _Decimal128 *x206; /* { dg-error "error" "unsigned int short _Decimal128" } */
unsigned int long _Decimal32 *x207; /* { dg-error "error" "unsigned int long _Decimal32" } */
unsigned int long _Decimal64 *x208; /* { dg-error "error" "unsigned int long _Decimal64" } */
unsigned int long _Decimal128 *x209; /* { dg-error "error" "unsigned int long _Decimal128" } */
unsigned long int _Decimal32 *x210; /* { dg-error "error" "unsigned long int _Decimal32" } */
unsigned long int _Decimal64 *x211; /* { dg-error "error" "unsigned long int _Decimal64" } */
unsigned long int _Decimal128 *x212; /* { dg-error "error" "unsigned long int _Decimal128" } */
unsigned long long _Decimal32 *x213; /* { dg-error "error" "unsigned long long _Decimal32" } */
unsigned long long _Decimal64 *x214; /* { dg-error "error" "unsigned long long _Decimal64" } */
unsigned long long _Decimal128 *x215; /* { dg-error "error" "unsigned long long _Decimal128" } */
int long long signed _Decimal32 *x216; /* { dg-error "error" "int long long signed _Decimal32" } */
int long long signed _Decimal64 *x217; /* { dg-error "error" "int long long signed _Decimal64" } */
int long long signed _Decimal128 *x218; /* { dg-error "error" "int long long signed _Decimal128" } */
int long long unsigned _Decimal32 *x219; /* { dg-error "error" "int long long unsigned _Decimal32" } */
int long long unsigned _Decimal64 *x220; /* { dg-error "error" "int long long unsigned _Decimal64" } */
int long long unsigned _Decimal128 *x221; /* { dg-error "error" "int long long unsigned _Decimal128" } */
int long signed long _Decimal32 *x222; /* { dg-error "error" "int long signed long _Decimal32" } */
int long signed long _Decimal64 *x223; /* { dg-error "error" "int long signed long _Decimal64" } */
int long signed long _Decimal128 *x224; /* { dg-error "error" "int long signed long _Decimal128" } */
int long unsigned long _Decimal32 *x226; /* { dg-error "error" "int long unsigned long _Decimal32" } */
int long unsigned long _Decimal64 *x227; /* { dg-error "error" "int long unsigned long _Decimal64" } */
int long unsigned long _Decimal128 *x228; /* { dg-error "error" "int long unsigned long _Decimal128" } */
int signed long long _Decimal32 *x229; /* { dg-error "error" "int signed long long _Decimal32" } */
int signed long long _Decimal64 *x230; /* { dg-error "error" "int signed long long _Decimal64" } */
int signed long long _Decimal128 *x231; /* { dg-error "error" "int signed long long _Decimal128" } */
int unsigned long long _Decimal32 *x232; /* { dg-error "error" "int unsigned long long _Decimal32" } */
int unsigned long long _Decimal64 *x233; /* { dg-error "error" "int unsigned long long _Decimal64" } */
int unsigned long long _Decimal128 *x234; /* { dg-error "error" "int unsigned long long _Decimal128" } */
long int long signed _Decimal32 *x235; /* { dg-error "error" "long int long signed _Decimal32" } */
long int long signed _Decimal64 *x236; /* { dg-error "error" "long int long signed _Decimal64" } */
long int long signed _Decimal128 *x237; /* { dg-error "error" "long int long signed _Decimal128" } */
long int long unsigned _Decimal32 *x238; /* { dg-error "error" "long int long unsigned _Decimal32" } */
long int long unsigned _Decimal64 *x239; /* { dg-error "error" "long int long unsigned _Decimal64" } */
long int long unsigned _Decimal128 *x240; /* { dg-error "error" "long int long unsigned _Decimal128" } */
long int signed long _Decimal32 *x241; /* { dg-error "error" "long int signed long _Decimal32" } */
long int signed long _Decimal64 *x242; /* { dg-error "error" "long int signed long _Decimal64" } */
long int signed long _Decimal128 *x243; /* { dg-error "error" "long int signed long _Decimal128" } */
long int unsigned long _Decimal32 *x244; /* { dg-error "error" "long int unsigned long _Decimal32" } */
long int unsigned long _Decimal64 *x245; /* { dg-error "error" "long int unsigned long _Decimal64" } */
long int unsigned long _Decimal128 *x246; /* { dg-error "error" "long int unsigned long _Decimal128" } */
long long int signed _Decimal32 *x247; /* { dg-error "error" "long long int signed _Decimal32" } */
long long int signed _Decimal64 *x248; /* { dg-error "error" "long long int signed _Decimal64" } */
long long int signed _Decimal128 *x249; /* { dg-error "error" "long long int signed _Decimal128" } */
long long int unsigned _Decimal32 *x250; /* { dg-error "error" "long long int unsigned _Decimal32" } */
long long int unsigned _Decimal64 *x251; /* { dg-error "error" "long long int unsigned _Decimal64" } */
long long int unsigned _Decimal128 *x252; /* { dg-error "error" "long long int unsigned _Decimal128" } */
long long signed int _Decimal32 *x253; /* { dg-error "error" "long long signed int _Decimal32" } */
long long signed int _Decimal64 *x254; /* { dg-error "error" "long long signed int _Decimal64" } */
long long signed int _Decimal128 *x255; /* { dg-error "error" "long long signed int _Decimal128" } */
long long unsigned int _Decimal32 *x256; /* { dg-error "error" "long long unsigned int _Decimal32" } */
long long unsigned int _Decimal64 *x257; /* { dg-error "error" "long long unsigned int _Decimal64" } */
long long unsigned int _Decimal128 *x258; /* { dg-error "error" "long long unsigned int _Decimal128" } */
long signed int long _Decimal32 *x259; /* { dg-error "error" "long signed int long _Decimal32" } */
long signed int long _Decimal64 *x260; /* { dg-error "error" "long signed int long _Decimal64" } */
long signed int long _Decimal128 *x261; /* { dg-error "error" "long signed int long _Decimal128" } */
long signed long int _Decimal32 *x262; /* { dg-error "error" "long signed long int _Decimal32" } */
long signed long int _Decimal64 *x263; /* { dg-error "error" "long signed long int _Decimal64" } */
long signed long int _Decimal128 *x264; /* { dg-error "error" "long signed long int _Decimal128" } */
long unsigned int long _Decimal32 *x265; /* { dg-error "error" "long unsigned int long _Decimal32" } */
long unsigned int long _Decimal64 *x266; /* { dg-error "error" "long unsigned int long _Decimal64" } */
long unsigned int long _Decimal128 *x267; /* { dg-error "error" "long unsigned int long _Decimal128" } */
long unsigned long int _Decimal32 *x268; /* { dg-error "error" "long unsigned long int _Decimal32" } */
long unsigned long int _Decimal64 *x269; /* { dg-error "error" "long unsigned long int _Decimal64" } */
long unsigned long int _Decimal128 *x270; /* { dg-error "error" "long unsigned long int _Decimal128" } */
signed long long int _Decimal32 *x271; /* { dg-error "error" "signed long long int _Decimal32" } */
signed long long int _Decimal64 *x272; /* { dg-error "error" "signed long long int _Decimal64" } */
signed long long int _Decimal128 *x273; /* { dg-error "error" "signed long long int _Decimal128" } */
unsigned int long long _Decimal32 *x274; /* { dg-error "error" "unsigned int long long _Decimal32" } */
unsigned int long long _Decimal64 *x275; /* { dg-error "error" "unsigned int long long _Decimal64" } */
unsigned int long long _Decimal128 *x276; /* { dg-error "error" "unsigned int long long _Decimal128" } */
unsigned long int long _Decimal32 *x277; /* { dg-error "error" "unsigned long int long _Decimal32" } */
unsigned long int long _Decimal64 *x278; /* { dg-error "error" "unsigned long int long _Decimal64" } */
unsigned long int long _Decimal128 *x279; /* { dg-error "error" "unsigned long int long _Decimal128" } */
unsigned long long int _Decimal32 *x280; /* { dg-error "error" "unsigned long long int _Decimal32" } */
unsigned long long int _Decimal64 *x281; /* { dg-error "error" "unsigned long long int _Decimal64" } */
unsigned long long int _Decimal128 *x282; /* { dg-error "error" "unsigned long long int _Decimal128" } */

_Complex _Decimal32 *x283; /* { dg-error "error" "_Complex _Decimal32" } */
_Complex _Decimal64 *x284; /* { dg-error "error" "_Complex _Decimal64" } */
_Complex _Decimal128 *x285; /* { dg-error "error" "_Complex _Decimal128" } */

_Decimal32 _Complex *x286; /* { dg-error "error" "_Decimal32 _Complex" } */
_Decimal64 _Complex *x287; /* { dg-error "error" "_Decimal64 _Complex" } */
_Decimal128 _Complex *x288; /* { dg-error "error" "_Decimal128 _Complex" } */

unsigned _Decimal32 *x289; /* { dg-error "error" "unsigned _Decimal32" } */
unsigned _Decimal64 *x290; /* { dg-error "error" "unsigned _Decimal64" } */
unsigned _Decimal128 *x291; /* { dg-error "error" "unsigned _Decimal128" } */
signed _Decimal32 *x292; /* { dg-error "error" "signed _Decimal32" } */
signed _Decimal64 *x293; /* { dg-error "error" "signed _Decimal64" } */
signed _Decimal128 *x294; /* { dg-error "error" "signed _Decimal128" } */
