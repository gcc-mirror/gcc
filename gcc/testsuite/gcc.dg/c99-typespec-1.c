/* Test for valid and invalid combinations of type specifiers in C99.
   Similar to typespec-1.c but with -pedantic-errors.
   Includes _Complex, but not _Imaginary (expected to be removed in TC2).  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef char type;
void *x0;
char *x1;
short *x2;
int *x3;
long *x4;
float *x5;
double *x6;
signed *x7;
unsigned *x8;
_Bool *x9;
_Complex *x10; /* { dg-error "" "_Complex" } */
type *x11;
void void *x12; /* { dg-error "" "void void" } */
void char *x13; /* { dg-error "" "void char" } */
void short *x14; /* { dg-error "" "void short" } */
void int *x15; /* { dg-error "" "void int" } */
void long *x16; /* { dg-error "" "void long" } */
void float *x17; /* { dg-error "" "void float" } */
void double *x18; /* { dg-error "" "void double" } */
void signed *x19; /* { dg-error "" "void signed" } */
void unsigned *x20; /* { dg-error "" "void unsigned" } */
void _Bool *x21; /* { dg-error "" "void _Bool" } */
void _Complex *x22; /* { dg-error "" "void _Complex" } */
char void *x23; /* { dg-error "" "char void" } */
char char *x24; /* { dg-error "" "char char" } */
char short *x25; /* { dg-error "" "char short" } */
char int *x26; /* { dg-error "" "char int" } */
char long *x27; /* { dg-error "" "char long" } */
char float *x28; /* { dg-error "" "char float" } */
char double *x29; /* { dg-error "" "char double" } */
char signed *x30;
char unsigned *x31;
char _Bool *x32; /* { dg-error "" "char _Bool" } */
char _Complex *x33; /* { dg-error "" "char _Complex" } */
short void *x34; /* { dg-error "" "short void" } */
short char *x35; /* { dg-error "" "short char" } */
short short *x36; /* { dg-error "" "short short" } */
short int *x37;
short long *x38; /* { dg-error "" "short long" } */
short float *x39; /* { dg-error "" "short float" } */
short double *x40; /* { dg-error "" "short double" } */
short signed *x41;
short unsigned *x42;
short _Bool *x43; /* { dg-error "" "short _Bool" } */
short _Complex *x44; /* { dg-error "" "short _Complex" } */
int void *x45; /* { dg-error "" "int void" } */
int char *x46; /* { dg-error "" "int char" } */
int short *x47;
int int *x48; /* { dg-error "" "int int" } */
int long *x49;
int float *x50; /* { dg-error "" "int float" } */
int double *x51; /* { dg-error "" "int double" } */
int signed *x52;
int unsigned *x53;
int _Bool *x54; /* { dg-error "" "int _Bool" } */
int _Complex *x55; /* { dg-error "" "int _Complex" } */
long void *x56; /* { dg-error "" "long void" } */
long char *x57; /* { dg-error "" "long char" } */
long short *x58; /* { dg-error "" "long short" } */
long int *x59;
long long *x60;
long float *x61; /* { dg-error "" "long float" } */
long double *x62;
long signed *x63;
long unsigned *x64;
long _Bool *x65; /* { dg-error "" "long _Bool" } */
long _Complex *x66; /* { dg-error "" "long _Complex" } */
float void *x67; /* { dg-error "" "float void" } */
float char *x68; /* { dg-error "" "float char" } */
float short *x69; /* { dg-error "" "float short" } */
float int *x70; /* { dg-error "" "float int" } */
float long *x71; /* { dg-error "" "float long" } */
float float *x72; /* { dg-error "" "float float" } */
float double *x73; /* { dg-error "" "float double" } */
float signed *x74; /* { dg-error "" "float signed" } */
float unsigned *x75; /* { dg-error "" "float unsigned" } */
float _Bool *x76; /* { dg-error "" "float _Bool" } */
float _Complex *x77;
double void *x78; /* { dg-error "" "double void" } */
double char *x79; /* { dg-error "" "double char" } */
double short *x80; /* { dg-error "" "double short" } */
double int *x81; /* { dg-error "" "double int" } */
double long *x82;
double float *x83; /* { dg-error "" "double float" } */
double double *x84; /* { dg-error "" "double double" } */
double signed *x85; /* { dg-error "" "double signed" } */
double unsigned *x86; /* { dg-error "" "double unsigned" } */
double _Bool *x87; /* { dg-error "" "double _Bool" } */
double _Complex *x88;
signed void *x89; /* { dg-error "" "signed void" } */
signed char *x90;
signed short *x91;
signed int *x92;
signed long *x93;
signed float *x94; /* { dg-error "" "signed float" } */
signed double *x95; /* { dg-error "" "signed double" } */
signed signed *x96; /* { dg-error "" "signed signed" } */
signed unsigned *x97; /* { dg-error "" "signed unsigned" } */
signed _Bool *x98; /* { dg-error "" "signed _Bool" } */
signed _Complex *x99; /* { dg-error "" "signed _Complex" } */
unsigned void *x100; /* { dg-error "" "unsigned void" } */
unsigned char *x101;
unsigned short *x102;
unsigned int *x103;
unsigned long *x104;
unsigned float *x105; /* { dg-error "" "unsigned float" } */
unsigned double *x106; /* { dg-error "" "unsigned double" } */
unsigned signed *x107; /* { dg-error "" "unsigned signed" } */
unsigned unsigned *x108; /* { dg-error "" "unsigned unsigned" } */
unsigned _Bool *x109; /* { dg-error "" "unsigned _Bool" } */
unsigned _Complex *x110; /* { dg-error "" "unsigned _Complex" } */
_Bool void *x111; /* { dg-error "" "_Bool void" } */
_Bool char *x112; /* { dg-error "" "_Bool char" } */
_Bool short *x113; /* { dg-error "" "_Bool short" } */
_Bool int *x114; /* { dg-error "" "_Bool int" } */
_Bool long *x115; /* { dg-error "" "_Bool long" } */
_Bool float *x116; /* { dg-error "" "_Bool float" } */
_Bool double *x117; /* { dg-error "" "_Bool double" } */
_Bool signed *x118; /* { dg-error "" "_Bool signed" } */
_Bool unsigned *x119; /* { dg-error "" "_Bool unsigned" } */
_Bool _Bool *x120; /* { dg-error "" "_Bool _Bool" } */
_Bool _Complex *x121; /* { dg-error "" "_Bool _Complex" } */
_Complex void *x122; /* { dg-error "" "_Complex void" } */
_Complex char *x123; /* { dg-error "" "_Complex char" } */
_Complex short *x124; /* { dg-error "" "_Complex short" } */
_Complex int *x125; /* { dg-error "" "_Complex int" } */
_Complex long *x126; /* { dg-error "" "_Complex long" } */
_Complex float *x127;
_Complex double *x128;
_Complex signed *x129; /* { dg-error "" "_Complex signed" } */
_Complex unsigned *x130; /* { dg-error "" "_Complex unsigned" } */
_Complex _Bool *x131; /* { dg-error "" "_Complex _Bool" } */
_Complex _Complex *x132; /* { dg-error "" "_Complex _Complex" } */
type void *x133; /* { dg-error "" "type void" } */
type char *x134; /* { dg-error "" "type char" } */
type short *x135; /* { dg-error "" "type short" } */
type int *x136; /* { dg-error "" "type int" } */
type long *x137; /* { dg-error "" "type long" } */
type float *x138; /* { dg-error "" "type float" } */
type double *x139; /* { dg-error "" "type double" } */
type signed *x140; /* { dg-error "" "type signed" } */
type unsigned *x141; /* { dg-error "" "type unsigned" } */
type _Bool *x142; /* { dg-error "" "type _Bool" } */
type _Complex *x143; /* { dg-error "" "type _Complex" } */
char signed void *x144; /* { dg-error "" "char signed void" } */
char signed char *x145; /* { dg-error "" "char signed char" } */
char signed short *x146; /* { dg-error "" "char signed short" } */
char signed int *x147; /* { dg-error "" "char signed int" } */
char signed long *x148; /* { dg-error "" "char signed long" } */
char signed float *x149; /* { dg-error "" "char signed float" } */
char signed double *x150; /* { dg-error "" "char signed double" } */
char signed signed *x151; /* { dg-error "" "char signed signed" } */
char signed unsigned *x152; /* { dg-error "" "char signed unsigned" } */
char signed _Bool *x153; /* { dg-error "" "char signed _Bool" } */
char signed _Complex *x154; /* { dg-error "" "char signed _Complex" } */
char unsigned void *x155; /* { dg-error "" "char unsigned void" } */
char unsigned char *x156; /* { dg-error "" "char unsigned char" } */
char unsigned short *x157; /* { dg-error "" "char unsigned short" } */
char unsigned int *x158; /* { dg-error "" "char unsigned int" } */
char unsigned long *x159; /* { dg-error "" "char unsigned long" } */
char unsigned float *x160; /* { dg-error "" "char unsigned float" } */
char unsigned double *x161; /* { dg-error "" "char unsigned double" } */
char unsigned signed *x162; /* { dg-error "" "char unsigned signed" } */
char unsigned unsigned *x163; /* { dg-error "" "char unsigned unsigned" } */
char unsigned _Bool *x164; /* { dg-error "" "char unsigned _Bool" } */
char unsigned _Complex *x165; /* { dg-error "" "char unsigned _Complex" } */
short int void *x166; /* { dg-error "" "short int void" } */
short int char *x167; /* { dg-error "" "short int char" } */
short int short *x168; /* { dg-error "" "short int short" } */
short int int *x169; /* { dg-error "" "short int int" } */
short int long *x170; /* { dg-error "" "short int long" } */
short int float *x171; /* { dg-error "" "short int float" } */
short int double *x172; /* { dg-error "" "short int double" } */
short int signed *x173;
short int unsigned *x174;
short int _Bool *x175; /* { dg-error "" "short int _Bool" } */
short int _Complex *x176; /* { dg-error "" "short int _Complex" } */
short signed void *x177; /* { dg-error "" "short signed void" } */
short signed char *x178; /* { dg-error "" "short signed char" } */
short signed short *x179; /* { dg-error "" "short signed short" } */
short signed int *x180;
short signed long *x181; /* { dg-error "" "short signed long" } */
short signed float *x182; /* { dg-error "" "short signed float" } */
short signed double *x183; /* { dg-error "" "short signed double" } */
short signed signed *x184; /* { dg-error "" "short signed signed" } */
short signed unsigned *x185; /* { dg-error "" "short signed unsigned" } */
short signed _Bool *x186; /* { dg-error "" "short signed _Bool" } */
short signed _Complex *x187; /* { dg-error "" "short signed _Complex" } */
short unsigned void *x188; /* { dg-error "" "short unsigned void" } */
short unsigned char *x189; /* { dg-error "" "short unsigned char" } */
short unsigned short *x190; /* { dg-error "" "short unsigned short" } */
short unsigned int *x191;
short unsigned long *x192; /* { dg-error "" "short unsigned long" } */
short unsigned float *x193; /* { dg-error "" "short unsigned float" } */
short unsigned double *x194; /* { dg-error "" "short unsigned double" } */
short unsigned signed *x195; /* { dg-error "" "short unsigned signed" } */
short unsigned unsigned *x196; /* { dg-error "" "short unsigned unsigned" } */
short unsigned _Bool *x197; /* { dg-error "" "short unsigned _Bool" } */
short unsigned _Complex *x198; /* { dg-error "" "short unsigned _Complex" } */
int short void *x199; /* { dg-error "" "int short void" } */
int short char *x200; /* { dg-error "" "int short char" } */
int short short *x201; /* { dg-error "" "int short short" } */
int short int *x202; /* { dg-error "" "int short int" } */
int short long *x203; /* { dg-error "" "int short long" } */
int short float *x204; /* { dg-error "" "int short float" } */
int short double *x205; /* { dg-error "" "int short double" } */
int short signed *x206;
int short unsigned *x207;
int short _Bool *x208; /* { dg-error "" "int short _Bool" } */
int short _Complex *x209; /* { dg-error "" "int short _Complex" } */
int long void *x210; /* { dg-error "" "int long void" } */
int long char *x211; /* { dg-error "" "int long char" } */
int long short *x212; /* { dg-error "" "int long short" } */
int long int *x213; /* { dg-error "" "int long int" } */
int long long *x214;
int long float *x215; /* { dg-error "" "int long float" } */
int long double *x216; /* { dg-error "" "int long double" } */
int long signed *x217;
int long unsigned *x218;
int long _Bool *x219; /* { dg-error "" "int long _Bool" } */
int long _Complex *x220; /* { dg-error "" "int long _Complex" } */
int signed void *x221; /* { dg-error "" "int signed void" } */
int signed char *x222; /* { dg-error "" "int signed char" } */
int signed short *x223;
int signed int *x224; /* { dg-error "" "int signed int" } */
int signed long *x225;
int signed float *x226; /* { dg-error "" "int signed float" } */
int signed double *x227; /* { dg-error "" "int signed double" } */
int signed signed *x228; /* { dg-error "" "int signed signed" } */
int signed unsigned *x229; /* { dg-error "" "int signed unsigned" } */
int signed _Bool *x230; /* { dg-error "" "int signed _Bool" } */
int signed _Complex *x231; /* { dg-error "" "int signed _Complex" } */
int unsigned void *x232; /* { dg-error "" "int unsigned void" } */
int unsigned char *x233; /* { dg-error "" "int unsigned char" } */
int unsigned short *x234;
int unsigned int *x235; /* { dg-error "" "int unsigned int" } */
int unsigned long *x236;
int unsigned float *x237; /* { dg-error "" "int unsigned float" } */
int unsigned double *x238; /* { dg-error "" "int unsigned double" } */
int unsigned signed *x239; /* { dg-error "" "int unsigned signed" } */
int unsigned unsigned *x240; /* { dg-error "" "int unsigned unsigned" } */
int unsigned _Bool *x241; /* { dg-error "" "int unsigned _Bool" } */
int unsigned _Complex *x242; /* { dg-error "" "int unsigned _Complex" } */
long int void *x243; /* { dg-error "" "long int void" } */
long int char *x244; /* { dg-error "" "long int char" } */
long int short *x245; /* { dg-error "" "long int short" } */
long int int *x246; /* { dg-error "" "long int int" } */
long int long *x247;
long int float *x248; /* { dg-error "" "long int float" } */
long int double *x249; /* { dg-error "" "long int double" } */
long int signed *x250;
long int unsigned *x251;
long int _Bool *x252; /* { dg-error "" "long int _Bool" } */
long int _Complex *x253; /* { dg-error "" "long int _Complex" } */
long long void *x254; /* { dg-error "" "long long void" } */
long long char *x255; /* { dg-error "" "long long char" } */
long long short *x256; /* { dg-error "" "long long short" } */
long long int *x257;
long long long *x258; /* { dg-error "" "long long long" } */
long long float *x259; /* { dg-error "" "long long float" } */
long long double *x260; /* { dg-error "" "long long double" } */
long long signed *x261;
long long unsigned *x262;
long long _Bool *x263; /* { dg-error "" "long long _Bool" } */
long long _Complex *x264; /* { dg-error "" "long long _Complex" } */
long double void *x265; /* { dg-error "" "long double void" } */
long double char *x266; /* { dg-error "" "long double char" } */
long double short *x267; /* { dg-error "" "long double short" } */
long double int *x268; /* { dg-error "" "long double int" } */
long double long *x269; /* { dg-error "" "long double long" } */
long double float *x270; /* { dg-error "" "long double float" } */
long double double *x271; /* { dg-error "" "long double double" } */
long double signed *x272; /* { dg-error "" "long double signed" } */
long double unsigned *x273; /* { dg-error "" "long double unsigned" } */
long double _Bool *x274; /* { dg-error "" "long double _Bool" } */
long double _Complex *x275;
long signed void *x276; /* { dg-error "" "long signed void" } */
long signed char *x277; /* { dg-error "" "long signed char" } */
long signed short *x278; /* { dg-error "" "long signed short" } */
long signed int *x279;
long signed long *x280;
long signed float *x281; /* { dg-error "" "long signed float" } */
long signed double *x282; /* { dg-error "" "long signed double" } */
long signed signed *x283; /* { dg-error "" "long signed signed" } */
long signed unsigned *x284; /* { dg-error "" "long signed unsigned" } */
long signed _Bool *x285; /* { dg-error "" "long signed _Bool" } */
long signed _Complex *x286; /* { dg-error "" "long signed _Complex" } */
long unsigned void *x287; /* { dg-error "" "long unsigned void" } */
long unsigned char *x288; /* { dg-error "" "long unsigned char" } */
long unsigned short *x289; /* { dg-error "" "long unsigned short" } */
long unsigned int *x290;
long unsigned long *x291;
long unsigned float *x292; /* { dg-error "" "long unsigned float" } */
long unsigned double *x293; /* { dg-error "" "long unsigned double" } */
long unsigned signed *x294; /* { dg-error "" "long unsigned signed" } */
long unsigned unsigned *x295; /* { dg-error "" "long unsigned unsigned" } */
long unsigned _Bool *x296; /* { dg-error "" "long unsigned _Bool" } */
long unsigned _Complex *x297; /* { dg-error "" "long unsigned _Complex" } */
long _Complex void *x298; /* { dg-error "" "long _Complex void" } */
long _Complex char *x299; /* { dg-error "" "long _Complex char" } */
long _Complex short *x300; /* { dg-error "" "long _Complex short" } */
long _Complex int *x301; /* { dg-error "" "long _Complex int" } */
long _Complex long *x302; /* { dg-error "" "long _Complex long" } */
long _Complex float *x303; /* { dg-error "" "long _Complex float" } */
long _Complex double *x304;
long _Complex signed *x305; /* { dg-error "" "long _Complex signed" } */
long _Complex unsigned *x306; /* { dg-error "" "long _Complex unsigned" } */
long _Complex _Bool *x307; /* { dg-error "" "long _Complex _Bool" } */
long _Complex _Complex *x308; /* { dg-error "" "long _Complex _Complex" } */
float _Complex void *x309; /* { dg-error "" "float _Complex void" } */
float _Complex char *x310; /* { dg-error "" "float _Complex char" } */
float _Complex short *x311; /* { dg-error "" "float _Complex short" } */
float _Complex int *x312; /* { dg-error "" "float _Complex int" } */
float _Complex long *x313; /* { dg-error "" "float _Complex long" } */
float _Complex float *x314; /* { dg-error "" "float _Complex float" } */
float _Complex double *x315; /* { dg-error "" "float _Complex double" } */
float _Complex signed *x316; /* { dg-error "" "float _Complex signed" } */
float _Complex unsigned *x317; /* { dg-error "" "float _Complex unsigned" } */
float _Complex _Bool *x318; /* { dg-error "" "float _Complex _Bool" } */
float _Complex _Complex *x319; /* { dg-error "" "float _Complex _Complex" } */
double long void *x320; /* { dg-error "" "double long void" } */
double long char *x321; /* { dg-error "" "double long char" } */
double long short *x322; /* { dg-error "" "double long short" } */
double long int *x323; /* { dg-error "" "double long int" } */
double long long *x324; /* { dg-error "" "double long long" } */
double long float *x325; /* { dg-error "" "double long float" } */
double long double *x326; /* { dg-error "" "double long double" } */
double long signed *x327; /* { dg-error "" "double long signed" } */
double long unsigned *x328; /* { dg-error "" "double long unsigned" } */
double long _Bool *x329; /* { dg-error "" "double long _Bool" } */
double long _Complex *x330;
double _Complex void *x331; /* { dg-error "" "double _Complex void" } */
double _Complex char *x332; /* { dg-error "" "double _Complex char" } */
double _Complex short *x333; /* { dg-error "" "double _Complex short" } */
double _Complex int *x334; /* { dg-error "" "double _Complex int" } */
double _Complex long *x335;
double _Complex float *x336; /* { dg-error "" "double _Complex float" } */
double _Complex double *x337; /* { dg-error "" "double _Complex double" } */
double _Complex signed *x338; /* { dg-error "" "double _Complex signed" } */
double _Complex unsigned *x339; /* { dg-error "" "double _Complex unsigned" } */
double _Complex _Bool *x340; /* { dg-error "" "double _Complex _Bool" } */
double _Complex _Complex *x341; /* { dg-error "" "double _Complex _Complex" } */
signed char void *x342; /* { dg-error "" "signed char void" } */
signed char char *x343; /* { dg-error "" "signed char char" } */
signed char short *x344; /* { dg-error "" "signed char short" } */
signed char int *x345; /* { dg-error "" "signed char int" } */
signed char long *x346; /* { dg-error "" "signed char long" } */
signed char float *x347; /* { dg-error "" "signed char float" } */
signed char double *x348; /* { dg-error "" "signed char double" } */
signed char signed *x349; /* { dg-error "" "signed char signed" } */
signed char unsigned *x350; /* { dg-error "" "signed char unsigned" } */
signed char _Bool *x351; /* { dg-error "" "signed char _Bool" } */
signed char _Complex *x352; /* { dg-error "" "signed char _Complex" } */
signed short void *x353; /* { dg-error "" "signed short void" } */
signed short char *x354; /* { dg-error "" "signed short char" } */
signed short short *x355; /* { dg-error "" "signed short short" } */
signed short int *x356;
signed short long *x357; /* { dg-error "" "signed short long" } */
signed short float *x358; /* { dg-error "" "signed short float" } */
signed short double *x359; /* { dg-error "" "signed short double" } */
signed short signed *x360; /* { dg-error "" "signed short signed" } */
signed short unsigned *x361; /* { dg-error "" "signed short unsigned" } */
signed short _Bool *x362; /* { dg-error "" "signed short _Bool" } */
signed short _Complex *x363; /* { dg-error "" "signed short _Complex" } */
signed int void *x364; /* { dg-error "" "signed int void" } */
signed int char *x365; /* { dg-error "" "signed int char" } */
signed int short *x366;
signed int int *x367; /* { dg-error "" "signed int int" } */
signed int long *x368;
signed int float *x369; /* { dg-error "" "signed int float" } */
signed int double *x370; /* { dg-error "" "signed int double" } */
signed int signed *x371; /* { dg-error "" "signed int signed" } */
signed int unsigned *x372; /* { dg-error "" "signed int unsigned" } */
signed int _Bool *x373; /* { dg-error "" "signed int _Bool" } */
signed int _Complex *x374; /* { dg-error "" "signed int _Complex" } */
signed long void *x375; /* { dg-error "" "signed long void" } */
signed long char *x376; /* { dg-error "" "signed long char" } */
signed long short *x377; /* { dg-error "" "signed long short" } */
signed long int *x378;
signed long long *x379;
signed long float *x380; /* { dg-error "" "signed long float" } */
signed long double *x381; /* { dg-error "" "signed long double" } */
signed long signed *x382; /* { dg-error "" "signed long signed" } */
signed long unsigned *x383; /* { dg-error "" "signed long unsigned" } */
signed long _Bool *x384; /* { dg-error "" "signed long _Bool" } */
signed long _Complex *x385; /* { dg-error "" "signed long _Complex" } */
unsigned char void *x386; /* { dg-error "" "unsigned char void" } */
unsigned char char *x387; /* { dg-error "" "unsigned char char" } */
unsigned char short *x388; /* { dg-error "" "unsigned char short" } */
unsigned char int *x389; /* { dg-error "" "unsigned char int" } */
unsigned char long *x390; /* { dg-error "" "unsigned char long" } */
unsigned char float *x391; /* { dg-error "" "unsigned char float" } */
unsigned char double *x392; /* { dg-error "" "unsigned char double" } */
unsigned char signed *x393; /* { dg-error "" "unsigned char signed" } */
unsigned char unsigned *x394; /* { dg-error "" "unsigned char unsigned" } */
unsigned char _Bool *x395; /* { dg-error "" "unsigned char _Bool" } */
unsigned char _Complex *x396; /* { dg-error "" "unsigned char _Complex" } */
unsigned short void *x397; /* { dg-error "" "unsigned short void" } */
unsigned short char *x398; /* { dg-error "" "unsigned short char" } */
unsigned short short *x399; /* { dg-error "" "unsigned short short" } */
unsigned short int *x400;
unsigned short long *x401; /* { dg-error "" "unsigned short long" } */
unsigned short float *x402; /* { dg-error "" "unsigned short float" } */
unsigned short double *x403; /* { dg-error "" "unsigned short double" } */
unsigned short signed *x404; /* { dg-error "" "unsigned short signed" } */
unsigned short unsigned *x405; /* { dg-error "" "unsigned short unsigned" } */
unsigned short _Bool *x406; /* { dg-error "" "unsigned short _Bool" } */
unsigned short _Complex *x407; /* { dg-error "" "unsigned short _Complex" } */
unsigned int void *x408; /* { dg-error "" "unsigned int void" } */
unsigned int char *x409; /* { dg-error "" "unsigned int char" } */
unsigned int short *x410;
unsigned int int *x411; /* { dg-error "" "unsigned int int" } */
unsigned int long *x412;
unsigned int float *x413; /* { dg-error "" "unsigned int float" } */
unsigned int double *x414; /* { dg-error "" "unsigned int double" } */
unsigned int signed *x415; /* { dg-error "" "unsigned int signed" } */
unsigned int unsigned *x416; /* { dg-error "" "unsigned int unsigned" } */
unsigned int _Bool *x417; /* { dg-error "" "unsigned int _Bool" } */
unsigned int _Complex *x418; /* { dg-error "" "unsigned int _Complex" } */
unsigned long void *x419; /* { dg-error "" "unsigned long void" } */
unsigned long char *x420; /* { dg-error "" "unsigned long char" } */
unsigned long short *x421; /* { dg-error "" "unsigned long short" } */
unsigned long int *x422;
unsigned long long *x423;
unsigned long float *x424; /* { dg-error "" "unsigned long float" } */
unsigned long double *x425; /* { dg-error "" "unsigned long double" } */
unsigned long signed *x426; /* { dg-error "" "unsigned long signed" } */
unsigned long unsigned *x427; /* { dg-error "" "unsigned long unsigned" } */
unsigned long _Bool *x428; /* { dg-error "" "unsigned long _Bool" } */
unsigned long _Complex *x429; /* { dg-error "" "unsigned long _Complex" } */
_Complex long void *x430; /* { dg-error "" "_Complex long void" } */
_Complex long char *x431; /* { dg-error "" "_Complex long char" } */
_Complex long short *x432; /* { dg-error "" "_Complex long short" } */
_Complex long int *x433; /* { dg-error "" "_Complex long int" } */
_Complex long long *x434; /* { dg-error "" "_Complex long long" } */
_Complex long float *x435; /* { dg-error "" "_Complex long float" } */
_Complex long double *x436;
_Complex long signed *x437; /* { dg-error "" "_Complex long signed" } */
_Complex long unsigned *x438; /* { dg-error "" "_Complex long unsigned" } */
_Complex long _Bool *x439; /* { dg-error "" "_Complex long _Bool" } */
_Complex long _Complex *x440; /* { dg-error "" "_Complex long _Complex" } */
_Complex float void *x441; /* { dg-error "" "_Complex float void" } */
_Complex float char *x442; /* { dg-error "" "_Complex float char" } */
_Complex float short *x443; /* { dg-error "" "_Complex float short" } */
_Complex float int *x444; /* { dg-error "" "_Complex float int" } */
_Complex float long *x445; /* { dg-error "" "_Complex float long" } */
_Complex float float *x446; /* { dg-error "" "_Complex float float" } */
_Complex float double *x447; /* { dg-error "" "_Complex float double" } */
_Complex float signed *x448; /* { dg-error "" "_Complex float signed" } */
_Complex float unsigned *x449; /* { dg-error "" "_Complex float unsigned" } */
_Complex float _Bool *x450; /* { dg-error "" "_Complex float _Bool" } */
_Complex float _Complex *x451; /* { dg-error "" "_Complex float _Complex" } */
_Complex double void *x452; /* { dg-error "" "_Complex double void" } */
_Complex double char *x453; /* { dg-error "" "_Complex double char" } */
_Complex double short *x454; /* { dg-error "" "_Complex double short" } */
_Complex double int *x455; /* { dg-error "" "_Complex double int" } */
_Complex double long *x456;
_Complex double float *x457; /* { dg-error "" "_Complex double float" } */
_Complex double double *x458; /* { dg-error "" "_Complex double double" } */
_Complex double signed *x459; /* { dg-error "" "_Complex double signed" } */
_Complex double unsigned *x460; /* { dg-error "" "_Complex double unsigned" } */
_Complex double _Bool *x461; /* { dg-error "" "_Complex double _Bool" } */
_Complex double _Complex *x462; /* { dg-error "" "_Complex double _Complex" } */
short int signed void *x463; /* { dg-error "" "short int signed void" } */
short int signed char *x464; /* { dg-error "" "short int signed char" } */
short int signed short *x465; /* { dg-error "" "short int signed short" } */
short int signed int *x466; /* { dg-error "" "short int signed int" } */
short int signed long *x467; /* { dg-error "" "short int signed long" } */
short int signed float *x468; /* { dg-error "" "short int signed float" } */
short int signed double *x469; /* { dg-error "" "short int signed double" } */
short int signed signed *x470; /* { dg-error "" "short int signed signed" } */
short int signed unsigned *x471; /* { dg-error "" "short int signed unsigned" } */
short int signed _Bool *x472; /* { dg-error "" "short int signed _Bool" } */
short int signed _Complex *x473; /* { dg-error "" "short int signed _Complex" } */
short int unsigned void *x474; /* { dg-error "" "short int unsigned void" } */
short int unsigned char *x475; /* { dg-error "" "short int unsigned char" } */
short int unsigned short *x476; /* { dg-error "" "short int unsigned short" } */
short int unsigned int *x477; /* { dg-error "" "short int unsigned int" } */
short int unsigned long *x478; /* { dg-error "" "short int unsigned long" } */
short int unsigned float *x479; /* { dg-error "" "short int unsigned float" } */
short int unsigned double *x480; /* { dg-error "" "short int unsigned double" } */
short int unsigned signed *x481; /* { dg-error "" "short int unsigned signed" } */
short int unsigned unsigned *x482; /* { dg-error "" "short int unsigned unsigned" } */
short int unsigned _Bool *x483; /* { dg-error "" "short int unsigned _Bool" } */
short int unsigned _Complex *x484; /* { dg-error "" "short int unsigned _Complex" } */
short signed int void *x485; /* { dg-error "" "short signed int void" } */
short signed int char *x486; /* { dg-error "" "short signed int char" } */
short signed int short *x487; /* { dg-error "" "short signed int short" } */
short signed int int *x488; /* { dg-error "" "short signed int int" } */
short signed int long *x489; /* { dg-error "" "short signed int long" } */
short signed int float *x490; /* { dg-error "" "short signed int float" } */
short signed int double *x491; /* { dg-error "" "short signed int double" } */
short signed int signed *x492; /* { dg-error "" "short signed int signed" } */
short signed int unsigned *x493; /* { dg-error "" "short signed int unsigned" } */
short signed int _Bool *x494; /* { dg-error "" "short signed int _Bool" } */
short signed int _Complex *x495; /* { dg-error "" "short signed int _Complex" } */
short unsigned int void *x496; /* { dg-error "" "short unsigned int void" } */
short unsigned int char *x497; /* { dg-error "" "short unsigned int char" } */
short unsigned int short *x498; /* { dg-error "" "short unsigned int short" } */
short unsigned int int *x499; /* { dg-error "" "short unsigned int int" } */
short unsigned int long *x500; /* { dg-error "" "short unsigned int long" } */
short unsigned int float *x501; /* { dg-error "" "short unsigned int float" } */
short unsigned int double *x502; /* { dg-error "" "short unsigned int double" } */
short unsigned int signed *x503; /* { dg-error "" "short unsigned int signed" } */
short unsigned int unsigned *x504; /* { dg-error "" "short unsigned int unsigned" } */
short unsigned int _Bool *x505; /* { dg-error "" "short unsigned int _Bool" } */
short unsigned int _Complex *x506; /* { dg-error "" "short unsigned int _Complex" } */
int short signed void *x507; /* { dg-error "" "int short signed void" } */
int short signed char *x508; /* { dg-error "" "int short signed char" } */
int short signed short *x509; /* { dg-error "" "int short signed short" } */
int short signed int *x510; /* { dg-error "" "int short signed int" } */
int short signed long *x511; /* { dg-error "" "int short signed long" } */
int short signed float *x512; /* { dg-error "" "int short signed float" } */
int short signed double *x513; /* { dg-error "" "int short signed double" } */
int short signed signed *x514; /* { dg-error "" "int short signed signed" } */
int short signed unsigned *x515; /* { dg-error "" "int short signed unsigned" } */
int short signed _Bool *x516; /* { dg-error "" "int short signed _Bool" } */
int short signed _Complex *x517; /* { dg-error "" "int short signed _Complex" } */
int short unsigned void *x518; /* { dg-error "" "int short unsigned void" } */
int short unsigned char *x519; /* { dg-error "" "int short unsigned char" } */
int short unsigned short *x520; /* { dg-error "" "int short unsigned short" } */
int short unsigned int *x521; /* { dg-error "" "int short unsigned int" } */
int short unsigned long *x522; /* { dg-error "" "int short unsigned long" } */
int short unsigned float *x523; /* { dg-error "" "int short unsigned float" } */
int short unsigned double *x524; /* { dg-error "" "int short unsigned double" } */
int short unsigned signed *x525; /* { dg-error "" "int short unsigned signed" } */
int short unsigned unsigned *x526; /* { dg-error "" "int short unsigned unsigned" } */
int short unsigned _Bool *x527; /* { dg-error "" "int short unsigned _Bool" } */
int short unsigned _Complex *x528; /* { dg-error "" "int short unsigned _Complex" } */
int long long void *x529; /* { dg-error "" "int long long void" } */
int long long char *x530; /* { dg-error "" "int long long char" } */
int long long short *x531; /* { dg-error "" "int long long short" } */
int long long int *x532; /* { dg-error "" "int long long int" } */
int long long long *x533; /* { dg-error "" "int long long long" } */
int long long float *x534; /* { dg-error "" "int long long float" } */
int long long double *x535; /* { dg-error "" "int long long double" } */
int long long signed *x536;
int long long unsigned *x537;
int long long _Bool *x538; /* { dg-error "" "int long long _Bool" } */
int long long _Complex *x539; /* { dg-error "" "int long long _Complex" } */
int long signed void *x540; /* { dg-error "" "int long signed void" } */
int long signed char *x541; /* { dg-error "" "int long signed char" } */
int long signed short *x542; /* { dg-error "" "int long signed short" } */
int long signed int *x543; /* { dg-error "" "int long signed int" } */
int long signed long *x544;
int long signed float *x545; /* { dg-error "" "int long signed float" } */
int long signed double *x546; /* { dg-error "" "int long signed double" } */
int long signed signed *x547; /* { dg-error "" "int long signed signed" } */
int long signed unsigned *x548; /* { dg-error "" "int long signed unsigned" } */
int long signed _Bool *x549; /* { dg-error "" "int long signed _Bool" } */
int long signed _Complex *x550; /* { dg-error "" "int long signed _Complex" } */
int long unsigned void *x551; /* { dg-error "" "int long unsigned void" } */
int long unsigned char *x552; /* { dg-error "" "int long unsigned char" } */
int long unsigned short *x553; /* { dg-error "" "int long unsigned short" } */
int long unsigned int *x554; /* { dg-error "" "int long unsigned int" } */
int long unsigned long *x555;
int long unsigned float *x556; /* { dg-error "" "int long unsigned float" } */
int long unsigned double *x557; /* { dg-error "" "int long unsigned double" } */
int long unsigned signed *x558; /* { dg-error "" "int long unsigned signed" } */
int long unsigned unsigned *x559; /* { dg-error "" "int long unsigned unsigned" } */
int long unsigned _Bool *x560; /* { dg-error "" "int long unsigned _Bool" } */
int long unsigned _Complex *x561; /* { dg-error "" "int long unsigned _Complex" } */
int signed short void *x562; /* { dg-error "" "int signed short void" } */
int signed short char *x563; /* { dg-error "" "int signed short char" } */
int signed short short *x564; /* { dg-error "" "int signed short short" } */
int signed short int *x565; /* { dg-error "" "int signed short int" } */
int signed short long *x566; /* { dg-error "" "int signed short long" } */
int signed short float *x567; /* { dg-error "" "int signed short float" } */
int signed short double *x568; /* { dg-error "" "int signed short double" } */
int signed short signed *x569; /* { dg-error "" "int signed short signed" } */
int signed short unsigned *x570; /* { dg-error "" "int signed short unsigned" } */
int signed short _Bool *x571; /* { dg-error "" "int signed short _Bool" } */
int signed short _Complex *x572; /* { dg-error "" "int signed short _Complex" } */
int signed long void *x573; /* { dg-error "" "int signed long void" } */
int signed long char *x574; /* { dg-error "" "int signed long char" } */
int signed long short *x575; /* { dg-error "" "int signed long short" } */
int signed long int *x576; /* { dg-error "" "int signed long int" } */
int signed long long *x577;
int signed long float *x578; /* { dg-error "" "int signed long float" } */
int signed long double *x579; /* { dg-error "" "int signed long double" } */
int signed long signed *x580; /* { dg-error "" "int signed long signed" } */
int signed long unsigned *x581; /* { dg-error "" "int signed long unsigned" } */
int signed long _Bool *x582; /* { dg-error "" "int signed long _Bool" } */
int signed long _Complex *x583; /* { dg-error "" "int signed long _Complex" } */
int unsigned short void *x584; /* { dg-error "" "int unsigned short void" } */
int unsigned short char *x585; /* { dg-error "" "int unsigned short char" } */
int unsigned short short *x586; /* { dg-error "" "int unsigned short short" } */
int unsigned short int *x587; /* { dg-error "" "int unsigned short int" } */
int unsigned short long *x588; /* { dg-error "" "int unsigned short long" } */
int unsigned short float *x589; /* { dg-error "" "int unsigned short float" } */
int unsigned short double *x590; /* { dg-error "" "int unsigned short double" } */
int unsigned short signed *x591; /* { dg-error "" "int unsigned short signed" } */
int unsigned short unsigned *x592; /* { dg-error "" "int unsigned short unsigned" } */
int unsigned short _Bool *x593; /* { dg-error "" "int unsigned short _Bool" } */
int unsigned short _Complex *x594; /* { dg-error "" "int unsigned short _Complex" } */
int unsigned long void *x595; /* { dg-error "" "int unsigned long void" } */
int unsigned long char *x596; /* { dg-error "" "int unsigned long char" } */
int unsigned long short *x597; /* { dg-error "" "int unsigned long short" } */
int unsigned long int *x598; /* { dg-error "" "int unsigned long int" } */
int unsigned long long *x599;
int unsigned long float *x600; /* { dg-error "" "int unsigned long float" } */
int unsigned long double *x601; /* { dg-error "" "int unsigned long double" } */
int unsigned long signed *x602; /* { dg-error "" "int unsigned long signed" } */
int unsigned long unsigned *x603; /* { dg-error "" "int unsigned long unsigned" } */
int unsigned long _Bool *x604; /* { dg-error "" "int unsigned long _Bool" } */
int unsigned long _Complex *x605; /* { dg-error "" "int unsigned long _Complex" } */
long int long void *x606; /* { dg-error "" "long int long void" } */
long int long char *x607; /* { dg-error "" "long int long char" } */
long int long short *x608; /* { dg-error "" "long int long short" } */
long int long int *x609; /* { dg-error "" "long int long int" } */
long int long long *x610; /* { dg-error "" "long int long long" } */
long int long float *x611; /* { dg-error "" "long int long float" } */
long int long double *x612; /* { dg-error "" "long int long double" } */
long int long signed *x613;
long int long unsigned *x614;
long int long _Bool *x615; /* { dg-error "" "long int long _Bool" } */
long int long _Complex *x616; /* { dg-error "" "long int long _Complex" } */
long int signed void *x617; /* { dg-error "" "long int signed void" } */
long int signed char *x618; /* { dg-error "" "long int signed char" } */
long int signed short *x619; /* { dg-error "" "long int signed short" } */
long int signed int *x620; /* { dg-error "" "long int signed int" } */
long int signed long *x621;
long int signed float *x622; /* { dg-error "" "long int signed float" } */
long int signed double *x623; /* { dg-error "" "long int signed double" } */
long int signed signed *x624; /* { dg-error "" "long int signed signed" } */
long int signed unsigned *x625; /* { dg-error "" "long int signed unsigned" } */
long int signed _Bool *x626; /* { dg-error "" "long int signed _Bool" } */
long int signed _Complex *x627; /* { dg-error "" "long int signed _Complex" } */
long int unsigned void *x628; /* { dg-error "" "long int unsigned void" } */
long int unsigned char *x629; /* { dg-error "" "long int unsigned char" } */
long int unsigned short *x630; /* { dg-error "" "long int unsigned short" } */
long int unsigned int *x631; /* { dg-error "" "long int unsigned int" } */
long int unsigned long *x632;
long int unsigned float *x633; /* { dg-error "" "long int unsigned float" } */
long int unsigned double *x634; /* { dg-error "" "long int unsigned double" } */
long int unsigned signed *x635; /* { dg-error "" "long int unsigned signed" } */
long int unsigned unsigned *x636; /* { dg-error "" "long int unsigned unsigned" } */
long int unsigned _Bool *x637; /* { dg-error "" "long int unsigned _Bool" } */
long int unsigned _Complex *x638; /* { dg-error "" "long int unsigned _Complex" } */
long long int void *x639; /* { dg-error "" "long long int void" } */
long long int char *x640; /* { dg-error "" "long long int char" } */
long long int short *x641; /* { dg-error "" "long long int short" } */
long long int int *x642; /* { dg-error "" "long long int int" } */
long long int long *x643; /* { dg-error "" "long long int long" } */
long long int float *x644; /* { dg-error "" "long long int float" } */
long long int double *x645; /* { dg-error "" "long long int double" } */
long long int signed *x646;
long long int unsigned *x647;
long long int _Bool *x648; /* { dg-error "" "long long int _Bool" } */
long long int _Complex *x649; /* { dg-error "" "long long int _Complex" } */
long long signed void *x650; /* { dg-error "" "long long signed void" } */
long long signed char *x651; /* { dg-error "" "long long signed char" } */
long long signed short *x652; /* { dg-error "" "long long signed short" } */
long long signed int *x653;
long long signed long *x654; /* { dg-error "" "long long signed long" } */
long long signed float *x655; /* { dg-error "" "long long signed float" } */
long long signed double *x656; /* { dg-error "" "long long signed double" } */
long long signed signed *x657; /* { dg-error "" "long long signed signed" } */
long long signed unsigned *x658; /* { dg-error "" "long long signed unsigned" } */
long long signed _Bool *x659; /* { dg-error "" "long long signed _Bool" } */
long long signed _Complex *x660; /* { dg-error "" "long long signed _Complex" } */
long long unsigned void *x661; /* { dg-error "" "long long unsigned void" } */
long long unsigned char *x662; /* { dg-error "" "long long unsigned char" } */
long long unsigned short *x663; /* { dg-error "" "long long unsigned short" } */
long long unsigned int *x664;
long long unsigned long *x665; /* { dg-error "" "long long unsigned long" } */
long long unsigned float *x666; /* { dg-error "" "long long unsigned float" } */
long long unsigned double *x667; /* { dg-error "" "long long unsigned double" } */
long long unsigned signed *x668; /* { dg-error "" "long long unsigned signed" } */
long long unsigned unsigned *x669; /* { dg-error "" "long long unsigned unsigned" } */
long long unsigned _Bool *x670; /* { dg-error "" "long long unsigned _Bool" } */
long long unsigned _Complex *x671; /* { dg-error "" "long long unsigned _Complex" } */
long double _Complex void *x672; /* { dg-error "" "long double _Complex void" } */
long double _Complex char *x673; /* { dg-error "" "long double _Complex char" } */
long double _Complex short *x674; /* { dg-error "" "long double _Complex short" } */
long double _Complex int *x675; /* { dg-error "" "long double _Complex int" } */
long double _Complex long *x676; /* { dg-error "" "long double _Complex long" } */
long double _Complex float *x677; /* { dg-error "" "long double _Complex float" } */
long double _Complex double *x678; /* { dg-error "" "long double _Complex double" } */
long double _Complex signed *x679; /* { dg-error "" "long double _Complex signed" } */
long double _Complex unsigned *x680; /* { dg-error "" "long double _Complex unsigned" } */
long double _Complex _Bool *x681; /* { dg-error "" "long double _Complex _Bool" } */
long double _Complex _Complex *x682; /* { dg-error "" "long double _Complex _Complex" } */
long signed int void *x683; /* { dg-error "" "long signed int void" } */
long signed int char *x684; /* { dg-error "" "long signed int char" } */
long signed int short *x685; /* { dg-error "" "long signed int short" } */
long signed int int *x686; /* { dg-error "" "long signed int int" } */
long signed int long *x687;
long signed int float *x688; /* { dg-error "" "long signed int float" } */
long signed int double *x689; /* { dg-error "" "long signed int double" } */
long signed int signed *x690; /* { dg-error "" "long signed int signed" } */
long signed int unsigned *x691; /* { dg-error "" "long signed int unsigned" } */
long signed int _Bool *x692; /* { dg-error "" "long signed int _Bool" } */
long signed int _Complex *x693; /* { dg-error "" "long signed int _Complex" } */
long signed long void *x694; /* { dg-error "" "long signed long void" } */
long signed long char *x695; /* { dg-error "" "long signed long char" } */
long signed long short *x696; /* { dg-error "" "long signed long short" } */
long signed long int *x697;
long signed long long *x698; /* { dg-error "" "long signed long long" } */
long signed long float *x699; /* { dg-error "" "long signed long float" } */
long signed long double *x700; /* { dg-error "" "long signed long double" } */
long signed long signed *x701; /* { dg-error "" "long signed long signed" } */
long signed long unsigned *x702; /* { dg-error "" "long signed long unsigned" } */
long signed long _Bool *x703; /* { dg-error "" "long signed long _Bool" } */
long signed long _Complex *x704; /* { dg-error "" "long signed long _Complex" } */
long unsigned int void *x705; /* { dg-error "" "long unsigned int void" } */
long unsigned int char *x706; /* { dg-error "" "long unsigned int char" } */
long unsigned int short *x707; /* { dg-error "" "long unsigned int short" } */
long unsigned int int *x708; /* { dg-error "" "long unsigned int int" } */
long unsigned int long *x709;
long unsigned int float *x710; /* { dg-error "" "long unsigned int float" } */
long unsigned int double *x711; /* { dg-error "" "long unsigned int double" } */
long unsigned int signed *x712; /* { dg-error "" "long unsigned int signed" } */
long unsigned int unsigned *x713; /* { dg-error "" "long unsigned int unsigned" } */
long unsigned int _Bool *x714; /* { dg-error "" "long unsigned int _Bool" } */
long unsigned int _Complex *x715; /* { dg-error "" "long unsigned int _Complex" } */
long unsigned long void *x716; /* { dg-error "" "long unsigned long void" } */
long unsigned long char *x717; /* { dg-error "" "long unsigned long char" } */
long unsigned long short *x718; /* { dg-error "" "long unsigned long short" } */
long unsigned long int *x719;
long unsigned long long *x720; /* { dg-error "" "long unsigned long long" } */
long unsigned long float *x721; /* { dg-error "" "long unsigned long float" } */
long unsigned long double *x722; /* { dg-error "" "long unsigned long double" } */
long unsigned long signed *x723; /* { dg-error "" "long unsigned long signed" } */
long unsigned long unsigned *x724; /* { dg-error "" "long unsigned long unsigned" } */
long unsigned long _Bool *x725; /* { dg-error "" "long unsigned long _Bool" } */
long unsigned long _Complex *x726; /* { dg-error "" "long unsigned long _Complex" } */
long _Complex double void *x727; /* { dg-error "" "long _Complex double void" } */
long _Complex double char *x728; /* { dg-error "" "long _Complex double char" } */
long _Complex double short *x729; /* { dg-error "" "long _Complex double short" } */
long _Complex double int *x730; /* { dg-error "" "long _Complex double int" } */
long _Complex double long *x731; /* { dg-error "" "long _Complex double long" } */
long _Complex double float *x732; /* { dg-error "" "long _Complex double float" } */
long _Complex double double *x733; /* { dg-error "" "long _Complex double double" } */
long _Complex double signed *x734; /* { dg-error "" "long _Complex double signed" } */
long _Complex double unsigned *x735; /* { dg-error "" "long _Complex double unsigned" } */
long _Complex double _Bool *x736; /* { dg-error "" "long _Complex double _Bool" } */
long _Complex double _Complex *x737; /* { dg-error "" "long _Complex double _Complex" } */
double long _Complex void *x738; /* { dg-error "" "double long _Complex void" } */
double long _Complex char *x739; /* { dg-error "" "double long _Complex char" } */
double long _Complex short *x740; /* { dg-error "" "double long _Complex short" } */
double long _Complex int *x741; /* { dg-error "" "double long _Complex int" } */
double long _Complex long *x742; /* { dg-error "" "double long _Complex long" } */
double long _Complex float *x743; /* { dg-error "" "double long _Complex float" } */
double long _Complex double *x744; /* { dg-error "" "double long _Complex double" } */
double long _Complex signed *x745; /* { dg-error "" "double long _Complex signed" } */
double long _Complex unsigned *x746; /* { dg-error "" "double long _Complex unsigned" } */
double long _Complex _Bool *x747; /* { dg-error "" "double long _Complex _Bool" } */
double long _Complex _Complex *x748; /* { dg-error "" "double long _Complex _Complex" } */
double _Complex long void *x749; /* { dg-error "" "double _Complex long void" } */
double _Complex long char *x750; /* { dg-error "" "double _Complex long char" } */
double _Complex long short *x751; /* { dg-error "" "double _Complex long short" } */
double _Complex long int *x752; /* { dg-error "" "double _Complex long int" } */
double _Complex long long *x753; /* { dg-error "" "double _Complex long long" } */
double _Complex long float *x754; /* { dg-error "" "double _Complex long float" } */
double _Complex long double *x755; /* { dg-error "" "double _Complex long double" } */
double _Complex long signed *x756; /* { dg-error "" "double _Complex long signed" } */
double _Complex long unsigned *x757; /* { dg-error "" "double _Complex long unsigned" } */
double _Complex long _Bool *x758; /* { dg-error "" "double _Complex long _Bool" } */
double _Complex long _Complex *x759; /* { dg-error "" "double _Complex long _Complex" } */
signed short int void *x760; /* { dg-error "" "signed short int void" } */
signed short int char *x761; /* { dg-error "" "signed short int char" } */
signed short int short *x762; /* { dg-error "" "signed short int short" } */
signed short int int *x763; /* { dg-error "" "signed short int int" } */
signed short int long *x764; /* { dg-error "" "signed short int long" } */
signed short int float *x765; /* { dg-error "" "signed short int float" } */
signed short int double *x766; /* { dg-error "" "signed short int double" } */
signed short int signed *x767; /* { dg-error "" "signed short int signed" } */
signed short int unsigned *x768; /* { dg-error "" "signed short int unsigned" } */
signed short int _Bool *x769; /* { dg-error "" "signed short int _Bool" } */
signed short int _Complex *x770; /* { dg-error "" "signed short int _Complex" } */
signed int short void *x771; /* { dg-error "" "signed int short void" } */
signed int short char *x772; /* { dg-error "" "signed int short char" } */
signed int short short *x773; /* { dg-error "" "signed int short short" } */
signed int short int *x774; /* { dg-error "" "signed int short int" } */
signed int short long *x775; /* { dg-error "" "signed int short long" } */
signed int short float *x776; /* { dg-error "" "signed int short float" } */
signed int short double *x777; /* { dg-error "" "signed int short double" } */
signed int short signed *x778; /* { dg-error "" "signed int short signed" } */
signed int short unsigned *x779; /* { dg-error "" "signed int short unsigned" } */
signed int short _Bool *x780; /* { dg-error "" "signed int short _Bool" } */
signed int short _Complex *x781; /* { dg-error "" "signed int short _Complex" } */
signed int long void *x782; /* { dg-error "" "signed int long void" } */
signed int long char *x783; /* { dg-error "" "signed int long char" } */
signed int long short *x784; /* { dg-error "" "signed int long short" } */
signed int long int *x785; /* { dg-error "" "signed int long int" } */
signed int long long *x786;
signed int long float *x787; /* { dg-error "" "signed int long float" } */
signed int long double *x788; /* { dg-error "" "signed int long double" } */
signed int long signed *x789; /* { dg-error "" "signed int long signed" } */
signed int long unsigned *x790; /* { dg-error "" "signed int long unsigned" } */
signed int long _Bool *x791; /* { dg-error "" "signed int long _Bool" } */
signed int long _Complex *x792; /* { dg-error "" "signed int long _Complex" } */
signed long int void *x793; /* { dg-error "" "signed long int void" } */
signed long int char *x794; /* { dg-error "" "signed long int char" } */
signed long int short *x795; /* { dg-error "" "signed long int short" } */
signed long int int *x796; /* { dg-error "" "signed long int int" } */
signed long int long *x797;
signed long int float *x798; /* { dg-error "" "signed long int float" } */
signed long int double *x799; /* { dg-error "" "signed long int double" } */
signed long int signed *x800; /* { dg-error "" "signed long int signed" } */
signed long int unsigned *x801; /* { dg-error "" "signed long int unsigned" } */
signed long int _Bool *x802; /* { dg-error "" "signed long int _Bool" } */
signed long int _Complex *x803; /* { dg-error "" "signed long int _Complex" } */
signed long long void *x804; /* { dg-error "" "signed long long void" } */
signed long long char *x805; /* { dg-error "" "signed long long char" } */
signed long long short *x806; /* { dg-error "" "signed long long short" } */
signed long long int *x807;
signed long long long *x808; /* { dg-error "" "signed long long long" } */
signed long long float *x809; /* { dg-error "" "signed long long float" } */
signed long long double *x810; /* { dg-error "" "signed long long double" } */
signed long long signed *x811; /* { dg-error "" "signed long long signed" } */
signed long long unsigned *x812; /* { dg-error "" "signed long long unsigned" } */
signed long long _Bool *x813; /* { dg-error "" "signed long long _Bool" } */
signed long long _Complex *x814; /* { dg-error "" "signed long long _Complex" } */
unsigned short int void *x815; /* { dg-error "" "unsigned short int void" } */
unsigned short int char *x816; /* { dg-error "" "unsigned short int char" } */
unsigned short int short *x817; /* { dg-error "" "unsigned short int short" } */
unsigned short int int *x818; /* { dg-error "" "unsigned short int int" } */
unsigned short int long *x819; /* { dg-error "" "unsigned short int long" } */
unsigned short int float *x820; /* { dg-error "" "unsigned short int float" } */
unsigned short int double *x821; /* { dg-error "" "unsigned short int double" } */
unsigned short int signed *x822; /* { dg-error "" "unsigned short int signed" } */
unsigned short int unsigned *x823; /* { dg-error "" "unsigned short int unsigned" } */
unsigned short int _Bool *x824; /* { dg-error "" "unsigned short int _Bool" } */
unsigned short int _Complex *x825; /* { dg-error "" "unsigned short int _Complex" } */
unsigned int short void *x826; /* { dg-error "" "unsigned int short void" } */
unsigned int short char *x827; /* { dg-error "" "unsigned int short char" } */
unsigned int short short *x828; /* { dg-error "" "unsigned int short short" } */
unsigned int short int *x829; /* { dg-error "" "unsigned int short int" } */
unsigned int short long *x830; /* { dg-error "" "unsigned int short long" } */
unsigned int short float *x831; /* { dg-error "" "unsigned int short float" } */
unsigned int short double *x832; /* { dg-error "" "unsigned int short double" } */
unsigned int short signed *x833; /* { dg-error "" "unsigned int short signed" } */
unsigned int short unsigned *x834; /* { dg-error "" "unsigned int short unsigned" } */
unsigned int short _Bool *x835; /* { dg-error "" "unsigned int short _Bool" } */
unsigned int short _Complex *x836; /* { dg-error "" "unsigned int short _Complex" } */
unsigned int long void *x837; /* { dg-error "" "unsigned int long void" } */
unsigned int long char *x838; /* { dg-error "" "unsigned int long char" } */
unsigned int long short *x839; /* { dg-error "" "unsigned int long short" } */
unsigned int long int *x840; /* { dg-error "" "unsigned int long int" } */
unsigned int long long *x841;
unsigned int long float *x842; /* { dg-error "" "unsigned int long float" } */
unsigned int long double *x843; /* { dg-error "" "unsigned int long double" } */
unsigned int long signed *x844; /* { dg-error "" "unsigned int long signed" } */
unsigned int long unsigned *x845; /* { dg-error "" "unsigned int long unsigned" } */
unsigned int long _Bool *x846; /* { dg-error "" "unsigned int long _Bool" } */
unsigned int long _Complex *x847; /* { dg-error "" "unsigned int long _Complex" } */
unsigned long int void *x848; /* { dg-error "" "unsigned long int void" } */
unsigned long int char *x849; /* { dg-error "" "unsigned long int char" } */
unsigned long int short *x850; /* { dg-error "" "unsigned long int short" } */
unsigned long int int *x851; /* { dg-error "" "unsigned long int int" } */
unsigned long int long *x852;
unsigned long int float *x853; /* { dg-error "" "unsigned long int float" } */
unsigned long int double *x854; /* { dg-error "" "unsigned long int double" } */
unsigned long int signed *x855; /* { dg-error "" "unsigned long int signed" } */
unsigned long int unsigned *x856; /* { dg-error "" "unsigned long int unsigned" } */
unsigned long int _Bool *x857; /* { dg-error "" "unsigned long int _Bool" } */
unsigned long int _Complex *x858; /* { dg-error "" "unsigned long int _Complex" } */
unsigned long long void *x859; /* { dg-error "" "unsigned long long void" } */
unsigned long long char *x860; /* { dg-error "" "unsigned long long char" } */
unsigned long long short *x861; /* { dg-error "" "unsigned long long short" } */
unsigned long long int *x862;
unsigned long long long *x863; /* { dg-error "" "unsigned long long long" } */
unsigned long long float *x864; /* { dg-error "" "unsigned long long float" } */
unsigned long long double *x865; /* { dg-error "" "unsigned long long double" } */
unsigned long long signed *x866; /* { dg-error "" "unsigned long long signed" } */
unsigned long long unsigned *x867; /* { dg-error "" "unsigned long long unsigned" } */
unsigned long long _Bool *x868; /* { dg-error "" "unsigned long long _Bool" } */
unsigned long long _Complex *x869; /* { dg-error "" "unsigned long long _Complex" } */
_Complex long double void *x870; /* { dg-error "" "_Complex long double void" } */
_Complex long double char *x871; /* { dg-error "" "_Complex long double char" } */
_Complex long double short *x872; /* { dg-error "" "_Complex long double short" } */
_Complex long double int *x873; /* { dg-error "" "_Complex long double int" } */
_Complex long double long *x874; /* { dg-error "" "_Complex long double long" } */
_Complex long double float *x875; /* { dg-error "" "_Complex long double float" } */
_Complex long double double *x876; /* { dg-error "" "_Complex long double double" } */
_Complex long double signed *x877; /* { dg-error "" "_Complex long double signed" } */
_Complex long double unsigned *x878; /* { dg-error "" "_Complex long double unsigned" } */
_Complex long double _Bool *x879; /* { dg-error "" "_Complex long double _Bool" } */
_Complex long double _Complex *x880; /* { dg-error "" "_Complex long double _Complex" } */
_Complex double long void *x881; /* { dg-error "" "_Complex double long void" } */
_Complex double long char *x882; /* { dg-error "" "_Complex double long char" } */
_Complex double long short *x883; /* { dg-error "" "_Complex double long short" } */
_Complex double long int *x884; /* { dg-error "" "_Complex double long int" } */
_Complex double long long *x885; /* { dg-error "" "_Complex double long long" } */
_Complex double long float *x886; /* { dg-error "" "_Complex double long float" } */
_Complex double long double *x887; /* { dg-error "" "_Complex double long double" } */
_Complex double long signed *x888; /* { dg-error "" "_Complex double long signed" } */
_Complex double long unsigned *x889; /* { dg-error "" "_Complex double long unsigned" } */
_Complex double long _Bool *x890; /* { dg-error "" "_Complex double long _Bool" } */
_Complex double long _Complex *x891; /* { dg-error "" "_Complex double long _Complex" } */
int long long signed void *x892; /* { dg-error "" "int long long signed void" } */
int long long signed char *x893; /* { dg-error "" "int long long signed char" } */
int long long signed short *x894; /* { dg-error "" "int long long signed short" } */
int long long signed int *x895; /* { dg-error "" "int long long signed int" } */
int long long signed long *x896; /* { dg-error "" "int long long signed long" } */
int long long signed float *x897; /* { dg-error "" "int long long signed float" } */
int long long signed double *x898; /* { dg-error "" "int long long signed double" } */
int long long signed signed *x899; /* { dg-error "" "int long long signed signed" } */
int long long signed unsigned *x900; /* { dg-error "" "int long long signed unsigned" } */
int long long signed _Bool *x901; /* { dg-error "" "int long long signed _Bool" } */
int long long signed _Complex *x902; /* { dg-error "" "int long long signed _Complex" } */
int long long unsigned void *x903; /* { dg-error "" "int long long unsigned void" } */
int long long unsigned char *x904; /* { dg-error "" "int long long unsigned char" } */
int long long unsigned short *x905; /* { dg-error "" "int long long unsigned short" } */
int long long unsigned int *x906; /* { dg-error "" "int long long unsigned int" } */
int long long unsigned long *x907; /* { dg-error "" "int long long unsigned long" } */
int long long unsigned float *x908; /* { dg-error "" "int long long unsigned float" } */
int long long unsigned double *x909; /* { dg-error "" "int long long unsigned double" } */
int long long unsigned signed *x910; /* { dg-error "" "int long long unsigned signed" } */
int long long unsigned unsigned *x911; /* { dg-error "" "int long long unsigned unsigned" } */
int long long unsigned _Bool *x912; /* { dg-error "" "int long long unsigned _Bool" } */
int long long unsigned _Complex *x913; /* { dg-error "" "int long long unsigned _Complex" } */
int long signed long void *x914; /* { dg-error "" "int long signed long void" } */
int long signed long char *x915; /* { dg-error "" "int long signed long char" } */
int long signed long short *x916; /* { dg-error "" "int long signed long short" } */
int long signed long int *x917; /* { dg-error "" "int long signed long int" } */
int long signed long long *x918; /* { dg-error "" "int long signed long long" } */
int long signed long float *x919; /* { dg-error "" "int long signed long float" } */
int long signed long double *x920; /* { dg-error "" "int long signed long double" } */
int long signed long signed *x921; /* { dg-error "" "int long signed long signed" } */
int long signed long unsigned *x922; /* { dg-error "" "int long signed long unsigned" } */
int long signed long _Bool *x923; /* { dg-error "" "int long signed long _Bool" } */
int long signed long _Complex *x924; /* { dg-error "" "int long signed long _Complex" } */
int long unsigned long void *x925; /* { dg-error "" "int long unsigned long void" } */
int long unsigned long char *x926; /* { dg-error "" "int long unsigned long char" } */
int long unsigned long short *x927; /* { dg-error "" "int long unsigned long short" } */
int long unsigned long int *x928; /* { dg-error "" "int long unsigned long int" } */
int long unsigned long long *x929; /* { dg-error "" "int long unsigned long long" } */
int long unsigned long float *x930; /* { dg-error "" "int long unsigned long float" } */
int long unsigned long double *x931; /* { dg-error "" "int long unsigned long double" } */
int long unsigned long signed *x932; /* { dg-error "" "int long unsigned long signed" } */
int long unsigned long unsigned *x933; /* { dg-error "" "int long unsigned long unsigned" } */
int long unsigned long _Bool *x934; /* { dg-error "" "int long unsigned long _Bool" } */
int long unsigned long _Complex *x935; /* { dg-error "" "int long unsigned long _Complex" } */
int signed long long void *x936; /* { dg-error "" "int signed long long void" } */
int signed long long char *x937; /* { dg-error "" "int signed long long char" } */
int signed long long short *x938; /* { dg-error "" "int signed long long short" } */
int signed long long int *x939; /* { dg-error "" "int signed long long int" } */
int signed long long long *x940; /* { dg-error "" "int signed long long long" } */
int signed long long float *x941; /* { dg-error "" "int signed long long float" } */
int signed long long double *x942; /* { dg-error "" "int signed long long double" } */
int signed long long signed *x943; /* { dg-error "" "int signed long long signed" } */
int signed long long unsigned *x944; /* { dg-error "" "int signed long long unsigned" } */
int signed long long _Bool *x945; /* { dg-error "" "int signed long long _Bool" } */
int signed long long _Complex *x946; /* { dg-error "" "int signed long long _Complex" } */
int unsigned long long void *x947; /* { dg-error "" "int unsigned long long void" } */
int unsigned long long char *x948; /* { dg-error "" "int unsigned long long char" } */
int unsigned long long short *x949; /* { dg-error "" "int unsigned long long short" } */
int unsigned long long int *x950; /* { dg-error "" "int unsigned long long int" } */
int unsigned long long long *x951; /* { dg-error "" "int unsigned long long long" } */
int unsigned long long float *x952; /* { dg-error "" "int unsigned long long float" } */
int unsigned long long double *x953; /* { dg-error "" "int unsigned long long double" } */
int unsigned long long signed *x954; /* { dg-error "" "int unsigned long long signed" } */
int unsigned long long unsigned *x955; /* { dg-error "" "int unsigned long long unsigned" } */
int unsigned long long _Bool *x956; /* { dg-error "" "int unsigned long long _Bool" } */
int unsigned long long _Complex *x957; /* { dg-error "" "int unsigned long long _Complex" } */
long int long signed void *x958; /* { dg-error "" "long int long signed void" } */
long int long signed char *x959; /* { dg-error "" "long int long signed char" } */
long int long signed short *x960; /* { dg-error "" "long int long signed short" } */
long int long signed int *x961; /* { dg-error "" "long int long signed int" } */
long int long signed long *x962; /* { dg-error "" "long int long signed long" } */
long int long signed float *x963; /* { dg-error "" "long int long signed float" } */
long int long signed double *x964; /* { dg-error "" "long int long signed double" } */
long int long signed signed *x965; /* { dg-error "" "long int long signed signed" } */
long int long signed unsigned *x966; /* { dg-error "" "long int long signed unsigned" } */
long int long signed _Bool *x967; /* { dg-error "" "long int long signed _Bool" } */
long int long signed _Complex *x968; /* { dg-error "" "long int long signed _Complex" } */
long int long unsigned void *x969; /* { dg-error "" "long int long unsigned void" } */
long int long unsigned char *x970; /* { dg-error "" "long int long unsigned char" } */
long int long unsigned short *x971; /* { dg-error "" "long int long unsigned short" } */
long int long unsigned int *x972; /* { dg-error "" "long int long unsigned int" } */
long int long unsigned long *x973; /* { dg-error "" "long int long unsigned long" } */
long int long unsigned float *x974; /* { dg-error "" "long int long unsigned float" } */
long int long unsigned double *x975; /* { dg-error "" "long int long unsigned double" } */
long int long unsigned signed *x976; /* { dg-error "" "long int long unsigned signed" } */
long int long unsigned unsigned *x977; /* { dg-error "" "long int long unsigned unsigned" } */
long int long unsigned _Bool *x978; /* { dg-error "" "long int long unsigned _Bool" } */
long int long unsigned _Complex *x979; /* { dg-error "" "long int long unsigned _Complex" } */
long int signed long void *x980; /* { dg-error "" "long int signed long void" } */
long int signed long char *x981; /* { dg-error "" "long int signed long char" } */
long int signed long short *x982; /* { dg-error "" "long int signed long short" } */
long int signed long int *x983; /* { dg-error "" "long int signed long int" } */
long int signed long long *x984; /* { dg-error "" "long int signed long long" } */
long int signed long float *x985; /* { dg-error "" "long int signed long float" } */
long int signed long double *x986; /* { dg-error "" "long int signed long double" } */
long int signed long signed *x987; /* { dg-error "" "long int signed long signed" } */
long int signed long unsigned *x988; /* { dg-error "" "long int signed long unsigned" } */
long int signed long _Bool *x989; /* { dg-error "" "long int signed long _Bool" } */
long int signed long _Complex *x990; /* { dg-error "" "long int signed long _Complex" } */
long int unsigned long void *x991; /* { dg-error "" "long int unsigned long void" } */
long int unsigned long char *x992; /* { dg-error "" "long int unsigned long char" } */
long int unsigned long short *x993; /* { dg-error "" "long int unsigned long short" } */
long int unsigned long int *x994; /* { dg-error "" "long int unsigned long int" } */
long int unsigned long long *x995; /* { dg-error "" "long int unsigned long long" } */
long int unsigned long float *x996; /* { dg-error "" "long int unsigned long float" } */
long int unsigned long double *x997; /* { dg-error "" "long int unsigned long double" } */
long int unsigned long signed *x998; /* { dg-error "" "long int unsigned long signed" } */
long int unsigned long unsigned *x999; /* { dg-error "" "long int unsigned long unsigned" } */
long int unsigned long _Bool *x1000; /* { dg-error "" "long int unsigned long _Bool" } */
long int unsigned long _Complex *x1001; /* { dg-error "" "long int unsigned long _Complex" } */
long long int signed void *x1002; /* { dg-error "" "long long int signed void" } */
long long int signed char *x1003; /* { dg-error "" "long long int signed char" } */
long long int signed short *x1004; /* { dg-error "" "long long int signed short" } */
long long int signed int *x1005; /* { dg-error "" "long long int signed int" } */
long long int signed long *x1006; /* { dg-error "" "long long int signed long" } */
long long int signed float *x1007; /* { dg-error "" "long long int signed float" } */
long long int signed double *x1008; /* { dg-error "" "long long int signed double" } */
long long int signed signed *x1009; /* { dg-error "" "long long int signed signed" } */
long long int signed unsigned *x1010; /* { dg-error "" "long long int signed unsigned" } */
long long int signed _Bool *x1011; /* { dg-error "" "long long int signed _Bool" } */
long long int signed _Complex *x1012; /* { dg-error "" "long long int signed _Complex" } */
long long int unsigned void *x1013; /* { dg-error "" "long long int unsigned void" } */
long long int unsigned char *x1014; /* { dg-error "" "long long int unsigned char" } */
long long int unsigned short *x1015; /* { dg-error "" "long long int unsigned short" } */
long long int unsigned int *x1016; /* { dg-error "" "long long int unsigned int" } */
long long int unsigned long *x1017; /* { dg-error "" "long long int unsigned long" } */
long long int unsigned float *x1018; /* { dg-error "" "long long int unsigned float" } */
long long int unsigned double *x1019; /* { dg-error "" "long long int unsigned double" } */
long long int unsigned signed *x1020; /* { dg-error "" "long long int unsigned signed" } */
long long int unsigned unsigned *x1021; /* { dg-error "" "long long int unsigned unsigned" } */
long long int unsigned _Bool *x1022; /* { dg-error "" "long long int unsigned _Bool" } */
long long int unsigned _Complex *x1023; /* { dg-error "" "long long int unsigned _Complex" } */
long long signed int void *x1024; /* { dg-error "" "long long signed int void" } */
long long signed int char *x1025; /* { dg-error "" "long long signed int char" } */
long long signed int short *x1026; /* { dg-error "" "long long signed int short" } */
long long signed int int *x1027; /* { dg-error "" "long long signed int int" } */
long long signed int long *x1028; /* { dg-error "" "long long signed int long" } */
long long signed int float *x1029; /* { dg-error "" "long long signed int float" } */
long long signed int double *x1030; /* { dg-error "" "long long signed int double" } */
long long signed int signed *x1031; /* { dg-error "" "long long signed int signed" } */
long long signed int unsigned *x1032; /* { dg-error "" "long long signed int unsigned" } */
long long signed int _Bool *x1033; /* { dg-error "" "long long signed int _Bool" } */
long long signed int _Complex *x1034; /* { dg-error "" "long long signed int _Complex" } */
long long unsigned int void *x1035; /* { dg-error "" "long long unsigned int void" } */
long long unsigned int char *x1036; /* { dg-error "" "long long unsigned int char" } */
long long unsigned int short *x1037; /* { dg-error "" "long long unsigned int short" } */
long long unsigned int int *x1038; /* { dg-error "" "long long unsigned int int" } */
long long unsigned int long *x1039; /* { dg-error "" "long long unsigned int long" } */
long long unsigned int float *x1040; /* { dg-error "" "long long unsigned int float" } */
long long unsigned int double *x1041; /* { dg-error "" "long long unsigned int double" } */
long long unsigned int signed *x1042; /* { dg-error "" "long long unsigned int signed" } */
long long unsigned int unsigned *x1043; /* { dg-error "" "long long unsigned int unsigned" } */
long long unsigned int _Bool *x1044; /* { dg-error "" "long long unsigned int _Bool" } */
long long unsigned int _Complex *x1045; /* { dg-error "" "long long unsigned int _Complex" } */
long signed int long void *x1046; /* { dg-error "" "long signed int long void" } */
long signed int long char *x1047; /* { dg-error "" "long signed int long char" } */
long signed int long short *x1048; /* { dg-error "" "long signed int long short" } */
long signed int long int *x1049; /* { dg-error "" "long signed int long int" } */
long signed int long long *x1050; /* { dg-error "" "long signed int long long" } */
long signed int long float *x1051; /* { dg-error "" "long signed int long float" } */
long signed int long double *x1052; /* { dg-error "" "long signed int long double" } */
long signed int long signed *x1053; /* { dg-error "" "long signed int long signed" } */
long signed int long unsigned *x1054; /* { dg-error "" "long signed int long unsigned" } */
long signed int long _Bool *x1055; /* { dg-error "" "long signed int long _Bool" } */
long signed int long _Complex *x1056; /* { dg-error "" "long signed int long _Complex" } */
long signed long int void *x1057; /* { dg-error "" "long signed long int void" } */
long signed long int char *x1058; /* { dg-error "" "long signed long int char" } */
long signed long int short *x1059; /* { dg-error "" "long signed long int short" } */
long signed long int int *x1060; /* { dg-error "" "long signed long int int" } */
long signed long int long *x1061; /* { dg-error "" "long signed long int long" } */
long signed long int float *x1062; /* { dg-error "" "long signed long int float" } */
long signed long int double *x1063; /* { dg-error "" "long signed long int double" } */
long signed long int signed *x1064; /* { dg-error "" "long signed long int signed" } */
long signed long int unsigned *x1065; /* { dg-error "" "long signed long int unsigned" } */
long signed long int _Bool *x1066; /* { dg-error "" "long signed long int _Bool" } */
long signed long int _Complex *x1067; /* { dg-error "" "long signed long int _Complex" } */
long unsigned int long void *x1068; /* { dg-error "" "long unsigned int long void" } */
long unsigned int long char *x1069; /* { dg-error "" "long unsigned int long char" } */
long unsigned int long short *x1070; /* { dg-error "" "long unsigned int long short" } */
long unsigned int long int *x1071; /* { dg-error "" "long unsigned int long int" } */
long unsigned int long long *x1072; /* { dg-error "" "long unsigned int long long" } */
long unsigned int long float *x1073; /* { dg-error "" "long unsigned int long float" } */
long unsigned int long double *x1074; /* { dg-error "" "long unsigned int long double" } */
long unsigned int long signed *x1075; /* { dg-error "" "long unsigned int long signed" } */
long unsigned int long unsigned *x1076; /* { dg-error "" "long unsigned int long unsigned" } */
long unsigned int long _Bool *x1077; /* { dg-error "" "long unsigned int long _Bool" } */
long unsigned int long _Complex *x1078; /* { dg-error "" "long unsigned int long _Complex" } */
long unsigned long int void *x1079; /* { dg-error "" "long unsigned long int void" } */
long unsigned long int char *x1080; /* { dg-error "" "long unsigned long int char" } */
long unsigned long int short *x1081; /* { dg-error "" "long unsigned long int short" } */
long unsigned long int int *x1082; /* { dg-error "" "long unsigned long int int" } */
long unsigned long int long *x1083; /* { dg-error "" "long unsigned long int long" } */
long unsigned long int float *x1084; /* { dg-error "" "long unsigned long int float" } */
long unsigned long int double *x1085; /* { dg-error "" "long unsigned long int double" } */
long unsigned long int signed *x1086; /* { dg-error "" "long unsigned long int signed" } */
long unsigned long int unsigned *x1087; /* { dg-error "" "long unsigned long int unsigned" } */
long unsigned long int _Bool *x1088; /* { dg-error "" "long unsigned long int _Bool" } */
long unsigned long int _Complex *x1089; /* { dg-error "" "long unsigned long int _Complex" } */
signed int long long void *x1090; /* { dg-error "" "signed int long long void" } */
signed int long long char *x1091; /* { dg-error "" "signed int long long char" } */
signed int long long short *x1092; /* { dg-error "" "signed int long long short" } */
signed int long long int *x1093; /* { dg-error "" "signed int long long int" } */
signed int long long long *x1094; /* { dg-error "" "signed int long long long" } */
signed int long long float *x1095; /* { dg-error "" "signed int long long float" } */
signed int long long double *x1096; /* { dg-error "" "signed int long long double" } */
signed int long long signed *x1097; /* { dg-error "" "signed int long long signed" } */
signed int long long unsigned *x1098; /* { dg-error "" "signed int long long unsigned" } */
signed int long long _Bool *x1099; /* { dg-error "" "signed int long long _Bool" } */
signed int long long _Complex *x1100; /* { dg-error "" "signed int long long _Complex" } */
signed long int long void *x1101; /* { dg-error "" "signed long int long void" } */
signed long int long char *x1102; /* { dg-error "" "signed long int long char" } */
signed long int long short *x1103; /* { dg-error "" "signed long int long short" } */
signed long int long int *x1104; /* { dg-error "" "signed long int long int" } */
signed long int long long *x1105; /* { dg-error "" "signed long int long long" } */
signed long int long float *x1106; /* { dg-error "" "signed long int long float" } */
signed long int long double *x1107; /* { dg-error "" "signed long int long double" } */
signed long int long signed *x1108; /* { dg-error "" "signed long int long signed" } */
signed long int long unsigned *x1109; /* { dg-error "" "signed long int long unsigned" } */
signed long int long _Bool *x1110; /* { dg-error "" "signed long int long _Bool" } */
signed long int long _Complex *x1111; /* { dg-error "" "signed long int long _Complex" } */
signed long long int void *x1112; /* { dg-error "" "signed long long int void" } */
signed long long int char *x1113; /* { dg-error "" "signed long long int char" } */
signed long long int short *x1114; /* { dg-error "" "signed long long int short" } */
signed long long int int *x1115; /* { dg-error "" "signed long long int int" } */
signed long long int long *x1116; /* { dg-error "" "signed long long int long" } */
signed long long int float *x1117; /* { dg-error "" "signed long long int float" } */
signed long long int double *x1118; /* { dg-error "" "signed long long int double" } */
signed long long int signed *x1119; /* { dg-error "" "signed long long int signed" } */
signed long long int unsigned *x1120; /* { dg-error "" "signed long long int unsigned" } */
signed long long int _Bool *x1121; /* { dg-error "" "signed long long int _Bool" } */
signed long long int _Complex *x1122; /* { dg-error "" "signed long long int _Complex" } */
unsigned int long long void *x1123; /* { dg-error "" "unsigned int long long void" } */
unsigned int long long char *x1124; /* { dg-error "" "unsigned int long long char" } */
unsigned int long long short *x1125; /* { dg-error "" "unsigned int long long short" } */
unsigned int long long int *x1126; /* { dg-error "" "unsigned int long long int" } */
unsigned int long long long *x1127; /* { dg-error "" "unsigned int long long long" } */
unsigned int long long float *x1128; /* { dg-error "" "unsigned int long long float" } */
unsigned int long long double *x1129; /* { dg-error "" "unsigned int long long double" } */
unsigned int long long signed *x1130; /* { dg-error "" "unsigned int long long signed" } */
unsigned int long long unsigned *x1131; /* { dg-error "" "unsigned int long long unsigned" } */
unsigned int long long _Bool *x1132; /* { dg-error "" "unsigned int long long _Bool" } */
unsigned int long long _Complex *x1133; /* { dg-error "" "unsigned int long long _Complex" } */
unsigned long int long void *x1134; /* { dg-error "" "unsigned long int long void" } */
unsigned long int long char *x1135; /* { dg-error "" "unsigned long int long char" } */
unsigned long int long short *x1136; /* { dg-error "" "unsigned long int long short" } */
unsigned long int long int *x1137; /* { dg-error "" "unsigned long int long int" } */
unsigned long int long long *x1138; /* { dg-error "" "unsigned long int long long" } */
unsigned long int long float *x1139; /* { dg-error "" "unsigned long int long float" } */
unsigned long int long double *x1140; /* { dg-error "" "unsigned long int long double" } */
unsigned long int long signed *x1141; /* { dg-error "" "unsigned long int long signed" } */
unsigned long int long unsigned *x1142; /* { dg-error "" "unsigned long int long unsigned" } */
unsigned long int long _Bool *x1143; /* { dg-error "" "unsigned long int long _Bool" } */
unsigned long int long _Complex *x1144; /* { dg-error "" "unsigned long int long _Complex" } */
unsigned long long int void *x1145; /* { dg-error "" "unsigned long long int void" } */
unsigned long long int char *x1146; /* { dg-error "" "unsigned long long int char" } */
unsigned long long int short *x1147; /* { dg-error "" "unsigned long long int short" } */
unsigned long long int int *x1148; /* { dg-error "" "unsigned long long int int" } */
unsigned long long int long *x1149; /* { dg-error "" "unsigned long long int long" } */
unsigned long long int float *x1150; /* { dg-error "" "unsigned long long int float" } */
unsigned long long int double *x1151; /* { dg-error "" "unsigned long long int double" } */
unsigned long long int signed *x1152; /* { dg-error "" "unsigned long long int signed" } */
unsigned long long int unsigned *x1153; /* { dg-error "" "unsigned long long int unsigned" } */
unsigned long long int _Bool *x1154; /* { dg-error "" "unsigned long long int _Bool" } */
unsigned long long int _Complex *x1155; /* { dg-error "" "unsigned long long int _Complex" } */
