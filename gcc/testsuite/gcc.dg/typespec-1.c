/* Test for valid and invalid combinations of type specifiers.
   This test covers combinations of the reserved word type specifiers
   other than _Complex and _Imaginary, and a typedef name as the first
   specifier only.  Within those constraints, there are no GCC extensions
   and what is accepted should be exactly what C99 permits.  All
   sequences are tested which do not have a shorter invalid initial
   subsequence.  Within the given constraints, there are no valid sequences
   with an invalid initial subsequence.

   In particular, the example of PR c/4319 should be rejected unconditionally,
   not requiring -pedantic.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

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
type *x10;
void void *x11; /* { dg-error "" "void void" } */
void char *x12; /* { dg-error "" "void char" } */
void short *x13; /* { dg-error "" "void short" } */
void int *x14; /* { dg-error "" "void int" } */
void long *x15; /* { dg-error "" "void long" } */
void float *x16; /* { dg-error "" "void float" } */
void double *x17; /* { dg-error "" "void double" } */
void signed *x18; /* { dg-error "" "void signed" } */
void unsigned *x19; /* { dg-error "" "void unsigned" } */
void _Bool *x20; /* { dg-error "" "void _Bool" } */
char void *x21; /* { dg-error "" "char void" } */
char char *x22; /* { dg-error "" "char char" } */
char short *x23; /* { dg-error "" "char short" } */
char int *x24; /* { dg-error "" "char int" } */
char long *x25; /* { dg-error "" "char long" } */
char float *x26; /* { dg-error "" "char float" } */
char double *x27; /* { dg-error "" "char double" } */
char signed *x28;
char unsigned *x29;
char _Bool *x30; /* { dg-error "" "char _Bool" } */
short void *x31; /* { dg-error "" "short void" } */
short char *x32; /* { dg-error "" "short char" } */
short short *x33; /* { dg-error "" "short short" } */
short int *x34;
short long *x35; /* { dg-error "" "short long" } */
short float *x36; /* { dg-error "" "short float" } */
short double *x37; /* { dg-error "" "short double" } */
short signed *x38;
short unsigned *x39;
short _Bool *x40; /* { dg-error "" "short _Bool" } */
int void *x41; /* { dg-error "" "int void" } */
int char *x42; /* { dg-error "" "int char" } */
int short *x43;
int int *x44; /* { dg-error "" "int int" } */
int long *x45;
int float *x46; /* { dg-error "" "int float" } */
int double *x47; /* { dg-error "" "int double" } */
int signed *x48;
int unsigned *x49;
int _Bool *x50; /* { dg-error "" "int _Bool" } */
long void *x51; /* { dg-error "" "long void" } */
long char *x52; /* { dg-error "" "long char" } */
long short *x53; /* { dg-error "" "long short" } */
long int *x54;
long long *x55;
long float *x56; /* { dg-error "" "long float" } */
long double *x57;
long signed *x58;
long unsigned *x59;
long _Bool *x60; /* { dg-error "" "long _Bool" } */
float void *x61; /* { dg-error "" "float void" } */
float char *x62; /* { dg-error "" "float char" } */
float short *x63; /* { dg-error "" "float short" } */
float int *x64; /* { dg-error "" "float int" } */
float long *x65; /* { dg-error "" "float long" } */
float float *x66; /* { dg-error "" "float float" } */
float double *x67; /* { dg-error "" "float double" } */
float signed *x68; /* { dg-error "" "float signed" } */
float unsigned *x69; /* { dg-error "" "float unsigned" } */
float _Bool *x70; /* { dg-error "" "float _Bool" } */
double void *x71; /* { dg-error "" "double void" } */
double char *x72; /* { dg-error "" "double char" } */
double short *x73; /* { dg-error "" "double short" } */
double int *x74; /* { dg-error "" "double int" } */
double long *x75;
double float *x76; /* { dg-error "" "double float" } */
double double *x77; /* { dg-error "" "double double" } */
double signed *x78; /* { dg-error "" "double signed" } */
double unsigned *x79; /* { dg-error "" "double unsigned" } */
double _Bool *x80; /* { dg-error "" "double _Bool" } */
signed void *x81; /* { dg-error "" "signed void" } */
signed char *x82;
signed short *x83;
signed int *x84;
signed long *x85;
signed float *x86; /* { dg-error "" "signed float" } */
signed double *x87; /* { dg-error "" "signed double" } */
signed signed *x88; /* { dg-error "" "signed signed" } */
signed unsigned *x89; /* { dg-error "" "signed unsigned" } */
signed _Bool *x90; /* { dg-error "" "signed _Bool" } */
unsigned void *x91; /* { dg-error "" "unsigned void" } */
unsigned char *x92;
unsigned short *x93;
unsigned int *x94;
unsigned long *x95;
unsigned float *x96; /* { dg-error "" "unsigned float" } */
unsigned double *x97; /* { dg-error "" "unsigned double" } */
unsigned signed *x98; /* { dg-error "" "unsigned signed" } */
unsigned unsigned *x99; /* { dg-error "" "unsigned unsigned" } */
unsigned _Bool *x100; /* { dg-error "" "unsigned _Bool" } */
_Bool void *x101; /* { dg-error "" "_Bool void" } */
_Bool char *x102; /* { dg-error "" "_Bool char" } */
_Bool short *x103; /* { dg-error "" "_Bool short" } */
_Bool int *x104; /* { dg-error "" "_Bool int" } */
_Bool long *x105; /* { dg-error "" "_Bool long" } */
_Bool float *x106; /* { dg-error "" "_Bool float" } */
_Bool double *x107; /* { dg-error "" "_Bool double" } */
_Bool signed *x108; /* { dg-error "" "_Bool signed" } */
_Bool unsigned *x109; /* { dg-error "" "_Bool unsigned" } */
_Bool _Bool *x110; /* { dg-error "" "_Bool _Bool" } */
type void *x111; /* { dg-error "" "type void" } */
type char *x112; /* { dg-error "" "type char" } */
type short *x113; /* { dg-error "" "type short" } */
type int *x114; /* { dg-error "" "type int" } */
type long *x115; /* { dg-error "" "type long" } */
type float *x116; /* { dg-error "" "type float" } */
type double *x117; /* { dg-error "" "type double" } */
type signed *x118; /* { dg-error "" "type signed" } */
type unsigned *x119; /* { dg-error "" "type unsigned" } */
type _Bool *x120; /* { dg-error "" "type _Bool" } */
char signed void *x121; /* { dg-error "" "char signed void" } */
char signed char *x122; /* { dg-error "" "char signed char" } */
char signed short *x123; /* { dg-error "" "char signed short" } */
char signed int *x124; /* { dg-error "" "char signed int" } */
char signed long *x125; /* { dg-error "" "char signed long" } */
char signed float *x126; /* { dg-error "" "char signed float" } */
char signed double *x127; /* { dg-error "" "char signed double" } */
char signed signed *x128; /* { dg-error "" "char signed signed" } */
char signed unsigned *x129; /* { dg-error "" "char signed unsigned" } */
char signed _Bool *x130; /* { dg-error "" "char signed _Bool" } */
char unsigned void *x131; /* { dg-error "" "char unsigned void" } */
char unsigned char *x132; /* { dg-error "" "char unsigned char" } */
char unsigned short *x133; /* { dg-error "" "char unsigned short" } */
char unsigned int *x134; /* { dg-error "" "char unsigned int" } */
char unsigned long *x135; /* { dg-error "" "char unsigned long" } */
char unsigned float *x136; /* { dg-error "" "char unsigned float" } */
char unsigned double *x137; /* { dg-error "" "char unsigned double" } */
char unsigned signed *x138; /* { dg-error "" "char unsigned signed" } */
char unsigned unsigned *x139; /* { dg-error "" "char unsigned unsigned" } */
char unsigned _Bool *x140; /* { dg-error "" "char unsigned _Bool" } */
short int void *x141; /* { dg-error "" "short int void" } */
short int char *x142; /* { dg-error "" "short int char" } */
short int short *x143; /* { dg-error "" "short int short" } */
short int int *x144; /* { dg-error "" "short int int" } */
short int long *x145; /* { dg-error "" "short int long" } */
short int float *x146; /* { dg-error "" "short int float" } */
short int double *x147; /* { dg-error "" "short int double" } */
short int signed *x148;
short int unsigned *x149;
short int _Bool *x150; /* { dg-error "" "short int _Bool" } */
short signed void *x151; /* { dg-error "" "short signed void" } */
short signed char *x152; /* { dg-error "" "short signed char" } */
short signed short *x153; /* { dg-error "" "short signed short" } */
short signed int *x154;
short signed long *x155; /* { dg-error "" "short signed long" } */
short signed float *x156; /* { dg-error "" "short signed float" } */
short signed double *x157; /* { dg-error "" "short signed double" } */
short signed signed *x158; /* { dg-error "" "short signed signed" } */
short signed unsigned *x159; /* { dg-error "" "short signed unsigned" } */
short signed _Bool *x160; /* { dg-error "" "short signed _Bool" } */
short unsigned void *x161; /* { dg-error "" "short unsigned void" } */
short unsigned char *x162; /* { dg-error "" "short unsigned char" } */
short unsigned short *x163; /* { dg-error "" "short unsigned short" } */
short unsigned int *x164;
short unsigned long *x165; /* { dg-error "" "short unsigned long" } */
short unsigned float *x166; /* { dg-error "" "short unsigned float" } */
short unsigned double *x167; /* { dg-error "" "short unsigned double" } */
short unsigned signed *x168; /* { dg-error "" "short unsigned signed" } */
short unsigned unsigned *x169; /* { dg-error "" "short unsigned unsigned" } */
short unsigned _Bool *x170; /* { dg-error "" "short unsigned _Bool" } */
int short void *x171; /* { dg-error "" "int short void" } */
int short char *x172; /* { dg-error "" "int short char" } */
int short short *x173; /* { dg-error "" "int short short" } */
int short int *x174; /* { dg-error "" "int short int" } */
int short long *x175; /* { dg-error "" "int short long" } */
int short float *x176; /* { dg-error "" "int short float" } */
int short double *x177; /* { dg-error "" "int short double" } */
int short signed *x178;
int short unsigned *x179;
int short _Bool *x180; /* { dg-error "" "int short _Bool" } */
int long void *x181; /* { dg-error "" "int long void" } */
int long char *x182; /* { dg-error "" "int long char" } */
int long short *x183; /* { dg-error "" "int long short" } */
int long int *x184; /* { dg-error "" "int long int" } */
int long long *x185;
int long float *x186; /* { dg-error "" "int long float" } */
int long double *x187; /* { dg-error "" "int long double" } */
int long signed *x188;
int long unsigned *x189;
int long _Bool *x190; /* { dg-error "" "int long _Bool" } */
int signed void *x191; /* { dg-error "" "int signed void" } */
int signed char *x192; /* { dg-error "" "int signed char" } */
int signed short *x193;
int signed int *x194; /* { dg-error "" "int signed int" } */
int signed long *x195;
int signed float *x196; /* { dg-error "" "int signed float" } */
int signed double *x197; /* { dg-error "" "int signed double" } */
int signed signed *x198; /* { dg-error "" "int signed signed" } */
int signed unsigned *x199; /* { dg-error "" "int signed unsigned" } */
int signed _Bool *x200; /* { dg-error "" "int signed _Bool" } */
int unsigned void *x201; /* { dg-error "" "int unsigned void" } */
int unsigned char *x202; /* { dg-error "" "int unsigned char" } */
int unsigned short *x203;
int unsigned int *x204; /* { dg-error "" "int unsigned int" } */
int unsigned long *x205;
int unsigned float *x206; /* { dg-error "" "int unsigned float" } */
int unsigned double *x207; /* { dg-error "" "int unsigned double" } */
int unsigned signed *x208; /* { dg-error "" "int unsigned signed" } */
int unsigned unsigned *x209; /* { dg-error "" "int unsigned unsigned" } */
int unsigned _Bool *x210; /* { dg-error "" "int unsigned _Bool" } */
long int void *x211; /* { dg-error "" "long int void" } */
long int char *x212; /* { dg-error "" "long int char" } */
long int short *x213; /* { dg-error "" "long int short" } */
long int int *x214; /* { dg-error "" "long int int" } */
long int long *x215;
long int float *x216; /* { dg-error "" "long int float" } */
long int double *x217; /* { dg-error "" "long int double" } */
long int signed *x218;
long int unsigned *x219;
long int _Bool *x220; /* { dg-error "" "long int _Bool" } */
long long void *x221; /* { dg-error "" "long long void" } */
long long char *x222; /* { dg-error "" "long long char" } */
long long short *x223; /* { dg-error "" "long long short" } */
long long int *x224;
long long long *x225; /* { dg-error "" "long long long" } */
long long float *x226; /* { dg-error "" "long long float" } */
long long double *x227; /* { dg-error "" "long long double" } */
long long signed *x228;
long long unsigned *x229;
long long _Bool *x230; /* { dg-error "" "long long _Bool" } */
long double void *x231; /* { dg-error "" "long double void" } */
long double char *x232; /* { dg-error "" "long double char" } */
long double short *x233; /* { dg-error "" "long double short" } */
long double int *x234; /* { dg-error "" "long double int" } */
long double long *x235; /* { dg-error "" "long double long" } */
long double float *x236; /* { dg-error "" "long double float" } */
long double double *x237; /* { dg-error "" "long double double" } */
long double signed *x238; /* { dg-error "" "long double signed" } */
long double unsigned *x239; /* { dg-error "" "long double unsigned" } */
long double _Bool *x240; /* { dg-error "" "long double _Bool" } */
long signed void *x241; /* { dg-error "" "long signed void" } */
long signed char *x242; /* { dg-error "" "long signed char" } */
long signed short *x243; /* { dg-error "" "long signed short" } */
long signed int *x244;
long signed long *x245;
long signed float *x246; /* { dg-error "" "long signed float" } */
long signed double *x247; /* { dg-error "" "long signed double" } */
long signed signed *x248; /* { dg-error "" "long signed signed" } */
long signed unsigned *x249; /* { dg-error "" "long signed unsigned" } */
long signed _Bool *x250; /* { dg-error "" "long signed _Bool" } */
long unsigned void *x251; /* { dg-error "" "long unsigned void" } */
long unsigned char *x252; /* { dg-error "" "long unsigned char" } */
long unsigned short *x253; /* { dg-error "" "long unsigned short" } */
long unsigned int *x254;
long unsigned long *x255;
long unsigned float *x256; /* { dg-error "" "long unsigned float" } */
long unsigned double *x257; /* { dg-error "" "long unsigned double" } */
long unsigned signed *x258; /* { dg-error "" "long unsigned signed" } */
long unsigned unsigned *x259; /* { dg-error "" "long unsigned unsigned" } */
long unsigned _Bool *x260; /* { dg-error "" "long unsigned _Bool" } */
double long void *x261; /* { dg-error "" "double long void" } */
double long char *x262; /* { dg-error "" "double long char" } */
double long short *x263; /* { dg-error "" "double long short" } */
double long int *x264; /* { dg-error "" "double long int" } */
double long long *x265; /* { dg-error "" "double long long" } */
double long float *x266; /* { dg-error "" "double long float" } */
double long double *x267; /* { dg-error "" "double long double" } */
double long signed *x268; /* { dg-error "" "double long signed" } */
double long unsigned *x269; /* { dg-error "" "double long unsigned" } */
double long _Bool *x270; /* { dg-error "" "double long _Bool" } */
signed char void *x271; /* { dg-error "" "signed char void" } */
signed char char *x272; /* { dg-error "" "signed char char" } */
signed char short *x273; /* { dg-error "" "signed char short" } */
signed char int *x274; /* { dg-error "" "signed char int" } */
signed char long *x275; /* { dg-error "" "signed char long" } */
signed char float *x276; /* { dg-error "" "signed char float" } */
signed char double *x277; /* { dg-error "" "signed char double" } */
signed char signed *x278; /* { dg-error "" "signed char signed" } */
signed char unsigned *x279; /* { dg-error "" "signed char unsigned" } */
signed char _Bool *x280; /* { dg-error "" "signed char _Bool" } */
signed short void *x281; /* { dg-error "" "signed short void" } */
signed short char *x282; /* { dg-error "" "signed short char" } */
signed short short *x283; /* { dg-error "" "signed short short" } */
signed short int *x284;
signed short long *x285; /* { dg-error "" "signed short long" } */
signed short float *x286; /* { dg-error "" "signed short float" } */
signed short double *x287; /* { dg-error "" "signed short double" } */
signed short signed *x288; /* { dg-error "" "signed short signed" } */
signed short unsigned *x289; /* { dg-error "" "signed short unsigned" } */
signed short _Bool *x290; /* { dg-error "" "signed short _Bool" } */
signed int void *x291; /* { dg-error "" "signed int void" } */
signed int char *x292; /* { dg-error "" "signed int char" } */
signed int short *x293;
signed int int *x294; /* { dg-error "" "signed int int" } */
signed int long *x295;
signed int float *x296; /* { dg-error "" "signed int float" } */
signed int double *x297; /* { dg-error "" "signed int double" } */
signed int signed *x298; /* { dg-error "" "signed int signed" } */
signed int unsigned *x299; /* { dg-error "" "signed int unsigned" } */
signed int _Bool *x300; /* { dg-error "" "signed int _Bool" } */
signed long void *x301; /* { dg-error "" "signed long void" } */
signed long char *x302; /* { dg-error "" "signed long char" } */
signed long short *x303; /* { dg-error "" "signed long short" } */
signed long int *x304;
signed long long *x305;
signed long float *x306; /* { dg-error "" "signed long float" } */
signed long double *x307; /* { dg-error "" "signed long double" } */
signed long signed *x308; /* { dg-error "" "signed long signed" } */
signed long unsigned *x309; /* { dg-error "" "signed long unsigned" } */
signed long _Bool *x310; /* { dg-error "" "signed long _Bool" } */
unsigned char void *x311; /* { dg-error "" "unsigned char void" } */
unsigned char char *x312; /* { dg-error "" "unsigned char char" } */
unsigned char short *x313; /* { dg-error "" "unsigned char short" } */
unsigned char int *x314; /* { dg-error "" "unsigned char int" } */
unsigned char long *x315; /* { dg-error "" "unsigned char long" } */
unsigned char float *x316; /* { dg-error "" "unsigned char float" } */
unsigned char double *x317; /* { dg-error "" "unsigned char double" } */
unsigned char signed *x318; /* { dg-error "" "unsigned char signed" } */
unsigned char unsigned *x319; /* { dg-error "" "unsigned char unsigned" } */
unsigned char _Bool *x320; /* { dg-error "" "unsigned char _Bool" } */
unsigned short void *x321; /* { dg-error "" "unsigned short void" } */
unsigned short char *x322; /* { dg-error "" "unsigned short char" } */
unsigned short short *x323; /* { dg-error "" "unsigned short short" } */
unsigned short int *x324;
unsigned short long *x325; /* { dg-error "" "unsigned short long" } */
unsigned short float *x326; /* { dg-error "" "unsigned short float" } */
unsigned short double *x327; /* { dg-error "" "unsigned short double" } */
unsigned short signed *x328; /* { dg-error "" "unsigned short signed" } */
unsigned short unsigned *x329; /* { dg-error "" "unsigned short unsigned" } */
unsigned short _Bool *x330; /* { dg-error "" "unsigned short _Bool" } */
unsigned int void *x331; /* { dg-error "" "unsigned int void" } */
unsigned int char *x332; /* { dg-error "" "unsigned int char" } */
unsigned int short *x333;
unsigned int int *x334; /* { dg-error "" "unsigned int int" } */
unsigned int long *x335;
unsigned int float *x336; /* { dg-error "" "unsigned int float" } */
unsigned int double *x337; /* { dg-error "" "unsigned int double" } */
unsigned int signed *x338; /* { dg-error "" "unsigned int signed" } */
unsigned int unsigned *x339; /* { dg-error "" "unsigned int unsigned" } */
unsigned int _Bool *x340; /* { dg-error "" "unsigned int _Bool" } */
unsigned long void *x341; /* { dg-error "" "unsigned long void" } */
unsigned long char *x342; /* { dg-error "" "unsigned long char" } */
unsigned long short *x343; /* { dg-error "" "unsigned long short" } */
unsigned long int *x344;
unsigned long long *x345;
unsigned long float *x346; /* { dg-error "" "unsigned long float" } */
unsigned long double *x347; /* { dg-error "" "unsigned long double" } */
unsigned long signed *x348; /* { dg-error "" "unsigned long signed" } */
unsigned long unsigned *x349; /* { dg-error "" "unsigned long unsigned" } */
unsigned long _Bool *x350; /* { dg-error "" "unsigned long _Bool" } */
short int signed void *x351; /* { dg-error "" "short int signed void" } */
short int signed char *x352; /* { dg-error "" "short int signed char" } */
short int signed short *x353; /* { dg-error "" "short int signed short" } */
short int signed int *x354; /* { dg-error "" "short int signed int" } */
short int signed long *x355; /* { dg-error "" "short int signed long" } */
short int signed float *x356; /* { dg-error "" "short int signed float" } */
short int signed double *x357; /* { dg-error "" "short int signed double" } */
short int signed signed *x358; /* { dg-error "" "short int signed signed" } */
short int signed unsigned *x359; /* { dg-error "" "short int signed unsigned" } */
short int signed _Bool *x360; /* { dg-error "" "short int signed _Bool" } */
short int unsigned void *x361; /* { dg-error "" "short int unsigned void" } */
short int unsigned char *x362; /* { dg-error "" "short int unsigned char" } */
short int unsigned short *x363; /* { dg-error "" "short int unsigned short" } */
short int unsigned int *x364; /* { dg-error "" "short int unsigned int" } */
short int unsigned long *x365; /* { dg-error "" "short int unsigned long" } */
short int unsigned float *x366; /* { dg-error "" "short int unsigned float" } */
short int unsigned double *x367; /* { dg-error "" "short int unsigned double" } */
short int unsigned signed *x368; /* { dg-error "" "short int unsigned signed" } */
short int unsigned unsigned *x369; /* { dg-error "" "short int unsigned unsigned" } */
short int unsigned _Bool *x370; /* { dg-error "" "short int unsigned _Bool" } */
short signed int void *x371; /* { dg-error "" "short signed int void" } */
short signed int char *x372; /* { dg-error "" "short signed int char" } */
short signed int short *x373; /* { dg-error "" "short signed int short" } */
short signed int int *x374; /* { dg-error "" "short signed int int" } */
short signed int long *x375; /* { dg-error "" "short signed int long" } */
short signed int float *x376; /* { dg-error "" "short signed int float" } */
short signed int double *x377; /* { dg-error "" "short signed int double" } */
short signed int signed *x378; /* { dg-error "" "short signed int signed" } */
short signed int unsigned *x379; /* { dg-error "" "short signed int unsigned" } */
short signed int _Bool *x380; /* { dg-error "" "short signed int _Bool" } */
short unsigned int void *x381; /* { dg-error "" "short unsigned int void" } */
short unsigned int char *x382; /* { dg-error "" "short unsigned int char" } */
short unsigned int short *x383; /* { dg-error "" "short unsigned int short" } */
short unsigned int int *x384; /* { dg-error "" "short unsigned int int" } */
short unsigned int long *x385; /* { dg-error "" "short unsigned int long" } */
short unsigned int float *x386; /* { dg-error "" "short unsigned int float" } */
short unsigned int double *x387; /* { dg-error "" "short unsigned int double" } */
short unsigned int signed *x388; /* { dg-error "" "short unsigned int signed" } */
short unsigned int unsigned *x389; /* { dg-error "" "short unsigned int unsigned" } */
short unsigned int _Bool *x390; /* { dg-error "" "short unsigned int _Bool" } */
int short signed void *x391; /* { dg-error "" "int short signed void" } */
int short signed char *x392; /* { dg-error "" "int short signed char" } */
int short signed short *x393; /* { dg-error "" "int short signed short" } */
int short signed int *x394; /* { dg-error "" "int short signed int" } */
int short signed long *x395; /* { dg-error "" "int short signed long" } */
int short signed float *x396; /* { dg-error "" "int short signed float" } */
int short signed double *x397; /* { dg-error "" "int short signed double" } */
int short signed signed *x398; /* { dg-error "" "int short signed signed" } */
int short signed unsigned *x399; /* { dg-error "" "int short signed unsigned" } */
int short signed _Bool *x400; /* { dg-error "" "int short signed _Bool" } */
int short unsigned void *x401; /* { dg-error "" "int short unsigned void" } */
int short unsigned char *x402; /* { dg-error "" "int short unsigned char" } */
int short unsigned short *x403; /* { dg-error "" "int short unsigned short" } */
int short unsigned int *x404; /* { dg-error "" "int short unsigned int" } */
int short unsigned long *x405; /* { dg-error "" "int short unsigned long" } */
int short unsigned float *x406; /* { dg-error "" "int short unsigned float" } */
int short unsigned double *x407; /* { dg-error "" "int short unsigned double" } */
int short unsigned signed *x408; /* { dg-error "" "int short unsigned signed" } */
int short unsigned unsigned *x409; /* { dg-error "" "int short unsigned unsigned" } */
int short unsigned _Bool *x410; /* { dg-error "" "int short unsigned _Bool" } */
int long long void *x411; /* { dg-error "" "int long long void" } */
int long long char *x412; /* { dg-error "" "int long long char" } */
int long long short *x413; /* { dg-error "" "int long long short" } */
int long long int *x414; /* { dg-error "" "int long long int" } */
int long long long *x415; /* { dg-error "" "int long long long" } */
int long long float *x416; /* { dg-error "" "int long long float" } */
int long long double *x417; /* { dg-error "" "int long long double" } */
int long long signed *x418;
int long long unsigned *x419;
int long long _Bool *x420; /* { dg-error "" "int long long _Bool" } */
int long signed void *x421; /* { dg-error "" "int long signed void" } */
int long signed char *x422; /* { dg-error "" "int long signed char" } */
int long signed short *x423; /* { dg-error "" "int long signed short" } */
int long signed int *x424; /* { dg-error "" "int long signed int" } */
int long signed long *x425;
int long signed float *x426; /* { dg-error "" "int long signed float" } */
int long signed double *x427; /* { dg-error "" "int long signed double" } */
int long signed signed *x428; /* { dg-error "" "int long signed signed" } */
int long signed unsigned *x429; /* { dg-error "" "int long signed unsigned" } */
int long signed _Bool *x430; /* { dg-error "" "int long signed _Bool" } */
int long unsigned void *x431; /* { dg-error "" "int long unsigned void" } */
int long unsigned char *x432; /* { dg-error "" "int long unsigned char" } */
int long unsigned short *x433; /* { dg-error "" "int long unsigned short" } */
int long unsigned int *x434; /* { dg-error "" "int long unsigned int" } */
int long unsigned long *x435;
int long unsigned float *x436; /* { dg-error "" "int long unsigned float" } */
int long unsigned double *x437; /* { dg-error "" "int long unsigned double" } */
int long unsigned signed *x438; /* { dg-error "" "int long unsigned signed" } */
int long unsigned unsigned *x439; /* { dg-error "" "int long unsigned unsigned" } */
int long unsigned _Bool *x440; /* { dg-error "" "int long unsigned _Bool" } */
int signed short void *x441; /* { dg-error "" "int signed short void" } */
int signed short char *x442; /* { dg-error "" "int signed short char" } */
int signed short short *x443; /* { dg-error "" "int signed short short" } */
int signed short int *x444; /* { dg-error "" "int signed short int" } */
int signed short long *x445; /* { dg-error "" "int signed short long" } */
int signed short float *x446; /* { dg-error "" "int signed short float" } */
int signed short double *x447; /* { dg-error "" "int signed short double" } */
int signed short signed *x448; /* { dg-error "" "int signed short signed" } */
int signed short unsigned *x449; /* { dg-error "" "int signed short unsigned" } */
int signed short _Bool *x450; /* { dg-error "" "int signed short _Bool" } */
int signed long void *x451; /* { dg-error "" "int signed long void" } */
int signed long char *x452; /* { dg-error "" "int signed long char" } */
int signed long short *x453; /* { dg-error "" "int signed long short" } */
int signed long int *x454; /* { dg-error "" "int signed long int" } */
int signed long long *x455;
int signed long float *x456; /* { dg-error "" "int signed long float" } */
int signed long double *x457; /* { dg-error "" "int signed long double" } */
int signed long signed *x458; /* { dg-error "" "int signed long signed" } */
int signed long unsigned *x459; /* { dg-error "" "int signed long unsigned" } */
int signed long _Bool *x460; /* { dg-error "" "int signed long _Bool" } */
int unsigned short void *x461; /* { dg-error "" "int unsigned short void" } */
int unsigned short char *x462; /* { dg-error "" "int unsigned short char" } */
int unsigned short short *x463; /* { dg-error "" "int unsigned short short" } */
int unsigned short int *x464; /* { dg-error "" "int unsigned short int" } */
int unsigned short long *x465; /* { dg-error "" "int unsigned short long" } */
int unsigned short float *x466; /* { dg-error "" "int unsigned short float" } */
int unsigned short double *x467; /* { dg-error "" "int unsigned short double" } */
int unsigned short signed *x468; /* { dg-error "" "int unsigned short signed" } */
int unsigned short unsigned *x469; /* { dg-error "" "int unsigned short unsigned" } */
int unsigned short _Bool *x470; /* { dg-error "" "int unsigned short _Bool" } */
int unsigned long void *x471; /* { dg-error "" "int unsigned long void" } */
int unsigned long char *x472; /* { dg-error "" "int unsigned long char" } */
int unsigned long short *x473; /* { dg-error "" "int unsigned long short" } */
int unsigned long int *x474; /* { dg-error "" "int unsigned long int" } */
int unsigned long long *x475;
int unsigned long float *x476; /* { dg-error "" "int unsigned long float" } */
int unsigned long double *x477; /* { dg-error "" "int unsigned long double" } */
int unsigned long signed *x478; /* { dg-error "" "int unsigned long signed" } */
int unsigned long unsigned *x479; /* { dg-error "" "int unsigned long unsigned" } */
int unsigned long _Bool *x480; /* { dg-error "" "int unsigned long _Bool" } */
long int long void *x481; /* { dg-error "" "long int long void" } */
long int long char *x482; /* { dg-error "" "long int long char" } */
long int long short *x483; /* { dg-error "" "long int long short" } */
long int long int *x484; /* { dg-error "" "long int long int" } */
long int long long *x485; /* { dg-error "" "long int long long" } */
long int long float *x486; /* { dg-error "" "long int long float" } */
long int long double *x487; /* { dg-error "" "long int long double" } */
long int long signed *x488;
long int long unsigned *x489;
long int long _Bool *x490; /* { dg-error "" "long int long _Bool" } */
long int signed void *x491; /* { dg-error "" "long int signed void" } */
long int signed char *x492; /* { dg-error "" "long int signed char" } */
long int signed short *x493; /* { dg-error "" "long int signed short" } */
long int signed int *x494; /* { dg-error "" "long int signed int" } */
long int signed long *x495;
long int signed float *x496; /* { dg-error "" "long int signed float" } */
long int signed double *x497; /* { dg-error "" "long int signed double" } */
long int signed signed *x498; /* { dg-error "" "long int signed signed" } */
long int signed unsigned *x499; /* { dg-error "" "long int signed unsigned" } */
long int signed _Bool *x500; /* { dg-error "" "long int signed _Bool" } */
long int unsigned void *x501; /* { dg-error "" "long int unsigned void" } */
long int unsigned char *x502; /* { dg-error "" "long int unsigned char" } */
long int unsigned short *x503; /* { dg-error "" "long int unsigned short" } */
long int unsigned int *x504; /* { dg-error "" "long int unsigned int" } */
long int unsigned long *x505;
long int unsigned float *x506; /* { dg-error "" "long int unsigned float" } */
long int unsigned double *x507; /* { dg-error "" "long int unsigned double" } */
long int unsigned signed *x508; /* { dg-error "" "long int unsigned signed" } */
long int unsigned unsigned *x509; /* { dg-error "" "long int unsigned unsigned" } */
long int unsigned _Bool *x510; /* { dg-error "" "long int unsigned _Bool" } */
long long int void *x511; /* { dg-error "" "long long int void" } */
long long int char *x512; /* { dg-error "" "long long int char" } */
long long int short *x513; /* { dg-error "" "long long int short" } */
long long int int *x514; /* { dg-error "" "long long int int" } */
long long int long *x515; /* { dg-error "" "long long int long" } */
long long int float *x516; /* { dg-error "" "long long int float" } */
long long int double *x517; /* { dg-error "" "long long int double" } */
long long int signed *x518;
long long int unsigned *x519;
long long int _Bool *x520; /* { dg-error "" "long long int _Bool" } */
long long signed void *x521; /* { dg-error "" "long long signed void" } */
long long signed char *x522; /* { dg-error "" "long long signed char" } */
long long signed short *x523; /* { dg-error "" "long long signed short" } */
long long signed int *x524;
long long signed long *x525; /* { dg-error "" "long long signed long" } */
long long signed float *x526; /* { dg-error "" "long long signed float" } */
long long signed double *x527; /* { dg-error "" "long long signed double" } */
long long signed signed *x528; /* { dg-error "" "long long signed signed" } */
long long signed unsigned *x529; /* { dg-error "" "long long signed unsigned" } */
long long signed _Bool *x530; /* { dg-error "" "long long signed _Bool" } */
long long unsigned void *x531; /* { dg-error "" "long long unsigned void" } */
long long unsigned char *x532; /* { dg-error "" "long long unsigned char" } */
long long unsigned short *x533; /* { dg-error "" "long long unsigned short" } */
long long unsigned int *x534;
long long unsigned long *x535; /* { dg-error "" "long long unsigned long" } */
long long unsigned float *x536; /* { dg-error "" "long long unsigned float" } */
long long unsigned double *x537; /* { dg-error "" "long long unsigned double" } */
long long unsigned signed *x538; /* { dg-error "" "long long unsigned signed" } */
long long unsigned unsigned *x539; /* { dg-error "" "long long unsigned unsigned" } */
long long unsigned _Bool *x540; /* { dg-error "" "long long unsigned _Bool" } */
long signed int void *x541; /* { dg-error "" "long signed int void" } */
long signed int char *x542; /* { dg-error "" "long signed int char" } */
long signed int short *x543; /* { dg-error "" "long signed int short" } */
long signed int int *x544; /* { dg-error "" "long signed int int" } */
long signed int long *x545;
long signed int float *x546; /* { dg-error "" "long signed int float" } */
long signed int double *x547; /* { dg-error "" "long signed int double" } */
long signed int signed *x548; /* { dg-error "" "long signed int signed" } */
long signed int unsigned *x549; /* { dg-error "" "long signed int unsigned" } */
long signed int _Bool *x550; /* { dg-error "" "long signed int _Bool" } */
long signed long void *x551; /* { dg-error "" "long signed long void" } */
long signed long char *x552; /* { dg-error "" "long signed long char" } */
long signed long short *x553; /* { dg-error "" "long signed long short" } */
long signed long int *x554;
long signed long long *x555; /* { dg-error "" "long signed long long" } */
long signed long float *x556; /* { dg-error "" "long signed long float" } */
long signed long double *x557; /* { dg-error "" "long signed long double" } */
long signed long signed *x558; /* { dg-error "" "long signed long signed" } */
long signed long unsigned *x559; /* { dg-error "" "long signed long unsigned" } */
long signed long _Bool *x560; /* { dg-error "" "long signed long _Bool" } */
long unsigned int void *x561; /* { dg-error "" "long unsigned int void" } */
long unsigned int char *x562; /* { dg-error "" "long unsigned int char" } */
long unsigned int short *x563; /* { dg-error "" "long unsigned int short" } */
long unsigned int int *x564; /* { dg-error "" "long unsigned int int" } */
long unsigned int long *x565;
long unsigned int float *x566; /* { dg-error "" "long unsigned int float" } */
long unsigned int double *x567; /* { dg-error "" "long unsigned int double" } */
long unsigned int signed *x568; /* { dg-error "" "long unsigned int signed" } */
long unsigned int unsigned *x569; /* { dg-error "" "long unsigned int unsigned" } */
long unsigned int _Bool *x570; /* { dg-error "" "long unsigned int _Bool" } */
long unsigned long void *x571; /* { dg-error "" "long unsigned long void" } */
long unsigned long char *x572; /* { dg-error "" "long unsigned long char" } */
long unsigned long short *x573; /* { dg-error "" "long unsigned long short" } */
long unsigned long int *x574;
long unsigned long long *x575; /* { dg-error "" "long unsigned long long" } */
long unsigned long float *x576; /* { dg-error "" "long unsigned long float" } */
long unsigned long double *x577; /* { dg-error "" "long unsigned long double" } */
long unsigned long signed *x578; /* { dg-error "" "long unsigned long signed" } */
long unsigned long unsigned *x579; /* { dg-error "" "long unsigned long unsigned" } */
long unsigned long _Bool *x580; /* { dg-error "" "long unsigned long _Bool" } */
signed short int void *x581; /* { dg-error "" "signed short int void" } */
signed short int char *x582; /* { dg-error "" "signed short int char" } */
signed short int short *x583; /* { dg-error "" "signed short int short" } */
signed short int int *x584; /* { dg-error "" "signed short int int" } */
signed short int long *x585; /* { dg-error "" "signed short int long" } */
signed short int float *x586; /* { dg-error "" "signed short int float" } */
signed short int double *x587; /* { dg-error "" "signed short int double" } */
signed short int signed *x588; /* { dg-error "" "signed short int signed" } */
signed short int unsigned *x589; /* { dg-error "" "signed short int unsigned" } */
signed short int _Bool *x590; /* { dg-error "" "signed short int _Bool" } */
signed int short void *x591; /* { dg-error "" "signed int short void" } */
signed int short char *x592; /* { dg-error "" "signed int short char" } */
signed int short short *x593; /* { dg-error "" "signed int short short" } */
signed int short int *x594; /* { dg-error "" "signed int short int" } */
signed int short long *x595; /* { dg-error "" "signed int short long" } */
signed int short float *x596; /* { dg-error "" "signed int short float" } */
signed int short double *x597; /* { dg-error "" "signed int short double" } */
signed int short signed *x598; /* { dg-error "" "signed int short signed" } */
signed int short unsigned *x599; /* { dg-error "" "signed int short unsigned" } */
signed int short _Bool *x600; /* { dg-error "" "signed int short _Bool" } */
signed int long void *x601; /* { dg-error "" "signed int long void" } */
signed int long char *x602; /* { dg-error "" "signed int long char" } */
signed int long short *x603; /* { dg-error "" "signed int long short" } */
signed int long int *x604; /* { dg-error "" "signed int long int" } */
signed int long long *x605;
signed int long float *x606; /* { dg-error "" "signed int long float" } */
signed int long double *x607; /* { dg-error "" "signed int long double" } */
signed int long signed *x608; /* { dg-error "" "signed int long signed" } */
signed int long unsigned *x609; /* { dg-error "" "signed int long unsigned" } */
signed int long _Bool *x610; /* { dg-error "" "signed int long _Bool" } */
signed long int void *x611; /* { dg-error "" "signed long int void" } */
signed long int char *x612; /* { dg-error "" "signed long int char" } */
signed long int short *x613; /* { dg-error "" "signed long int short" } */
signed long int int *x614; /* { dg-error "" "signed long int int" } */
signed long int long *x615;
signed long int float *x616; /* { dg-error "" "signed long int float" } */
signed long int double *x617; /* { dg-error "" "signed long int double" } */
signed long int signed *x618; /* { dg-error "" "signed long int signed" } */
signed long int unsigned *x619; /* { dg-error "" "signed long int unsigned" } */
signed long int _Bool *x620; /* { dg-error "" "signed long int _Bool" } */
signed long long void *x621; /* { dg-error "" "signed long long void" } */
signed long long char *x622; /* { dg-error "" "signed long long char" } */
signed long long short *x623; /* { dg-error "" "signed long long short" } */
signed long long int *x624;
signed long long long *x625; /* { dg-error "" "signed long long long" } */
signed long long float *x626; /* { dg-error "" "signed long long float" } */
signed long long double *x627; /* { dg-error "" "signed long long double" } */
signed long long signed *x628; /* { dg-error "" "signed long long signed" } */
signed long long unsigned *x629; /* { dg-error "" "signed long long unsigned" } */
signed long long _Bool *x630; /* { dg-error "" "signed long long _Bool" } */
unsigned short int void *x631; /* { dg-error "" "unsigned short int void" } */
unsigned short int char *x632; /* { dg-error "" "unsigned short int char" } */
unsigned short int short *x633; /* { dg-error "" "unsigned short int short" } */
unsigned short int int *x634; /* { dg-error "" "unsigned short int int" } */
unsigned short int long *x635; /* { dg-error "" "unsigned short int long" } */
unsigned short int float *x636; /* { dg-error "" "unsigned short int float" } */
unsigned short int double *x637; /* { dg-error "" "unsigned short int double" } */
unsigned short int signed *x638; /* { dg-error "" "unsigned short int signed" } */
unsigned short int unsigned *x639; /* { dg-error "" "unsigned short int unsigned" } */
unsigned short int _Bool *x640; /* { dg-error "" "unsigned short int _Bool" } */
unsigned int short void *x641; /* { dg-error "" "unsigned int short void" } */
unsigned int short char *x642; /* { dg-error "" "unsigned int short char" } */
unsigned int short short *x643; /* { dg-error "" "unsigned int short short" } */
unsigned int short int *x644; /* { dg-error "" "unsigned int short int" } */
unsigned int short long *x645; /* { dg-error "" "unsigned int short long" } */
unsigned int short float *x646; /* { dg-error "" "unsigned int short float" } */
unsigned int short double *x647; /* { dg-error "" "unsigned int short double" } */
unsigned int short signed *x648; /* { dg-error "" "unsigned int short signed" } */
unsigned int short unsigned *x649; /* { dg-error "" "unsigned int short unsigned" } */
unsigned int short _Bool *x650; /* { dg-error "" "unsigned int short _Bool" } */
unsigned int long void *x651; /* { dg-error "" "unsigned int long void" } */
unsigned int long char *x652; /* { dg-error "" "unsigned int long char" } */
unsigned int long short *x653; /* { dg-error "" "unsigned int long short" } */
unsigned int long int *x654; /* { dg-error "" "unsigned int long int" } */
unsigned int long long *x655;
unsigned int long float *x656; /* { dg-error "" "unsigned int long float" } */
unsigned int long double *x657; /* { dg-error "" "unsigned int long double" } */
unsigned int long signed *x658; /* { dg-error "" "unsigned int long signed" } */
unsigned int long unsigned *x659; /* { dg-error "" "unsigned int long unsigned" } */
unsigned int long _Bool *x660; /* { dg-error "" "unsigned int long _Bool" } */
unsigned long int void *x661; /* { dg-error "" "unsigned long int void" } */
unsigned long int char *x662; /* { dg-error "" "unsigned long int char" } */
unsigned long int short *x663; /* { dg-error "" "unsigned long int short" } */
unsigned long int int *x664; /* { dg-error "" "unsigned long int int" } */
unsigned long int long *x665;
unsigned long int float *x666; /* { dg-error "" "unsigned long int float" } */
unsigned long int double *x667; /* { dg-error "" "unsigned long int double" } */
unsigned long int signed *x668; /* { dg-error "" "unsigned long int signed" } */
unsigned long int unsigned *x669; /* { dg-error "" "unsigned long int unsigned" } */
unsigned long int _Bool *x670; /* { dg-error "" "unsigned long int _Bool" } */
unsigned long long void *x671; /* { dg-error "" "unsigned long long void" } */
unsigned long long char *x672; /* { dg-error "" "unsigned long long char" } */
unsigned long long short *x673; /* { dg-error "" "unsigned long long short" } */
unsigned long long int *x674;
unsigned long long long *x675; /* { dg-error "" "unsigned long long long" } */
unsigned long long float *x676; /* { dg-error "" "unsigned long long float" } */
unsigned long long double *x677; /* { dg-error "" "unsigned long long double" } */
unsigned long long signed *x678; /* { dg-error "" "unsigned long long signed" } */
unsigned long long unsigned *x679; /* { dg-error "" "unsigned long long unsigned" } */
unsigned long long _Bool *x680; /* { dg-error "" "unsigned long long _Bool" } */
int long long signed void *x681; /* { dg-error "" "int long long signed void" } */
int long long signed char *x682; /* { dg-error "" "int long long signed char" } */
int long long signed short *x683; /* { dg-error "" "int long long signed short" } */
int long long signed int *x684; /* { dg-error "" "int long long signed int" } */
int long long signed long *x685; /* { dg-error "" "int long long signed long" } */
int long long signed float *x686; /* { dg-error "" "int long long signed float" } */
int long long signed double *x687; /* { dg-error "" "int long long signed double" } */
int long long signed signed *x688; /* { dg-error "" "int long long signed signed" } */
int long long signed unsigned *x689; /* { dg-error "" "int long long signed unsigned" } */
int long long signed _Bool *x690; /* { dg-error "" "int long long signed _Bool" } */
int long long unsigned void *x691; /* { dg-error "" "int long long unsigned void" } */
int long long unsigned char *x692; /* { dg-error "" "int long long unsigned char" } */
int long long unsigned short *x693; /* { dg-error "" "int long long unsigned short" } */
int long long unsigned int *x694; /* { dg-error "" "int long long unsigned int" } */
int long long unsigned long *x695; /* { dg-error "" "int long long unsigned long" } */
int long long unsigned float *x696; /* { dg-error "" "int long long unsigned float" } */
int long long unsigned double *x697; /* { dg-error "" "int long long unsigned double" } */
int long long unsigned signed *x698; /* { dg-error "" "int long long unsigned signed" } */
int long long unsigned unsigned *x699; /* { dg-error "" "int long long unsigned unsigned" } */
int long long unsigned _Bool *x700; /* { dg-error "" "int long long unsigned _Bool" } */
int long signed long void *x701; /* { dg-error "" "int long signed long void" } */
int long signed long char *x702; /* { dg-error "" "int long signed long char" } */
int long signed long short *x703; /* { dg-error "" "int long signed long short" } */
int long signed long int *x704; /* { dg-error "" "int long signed long int" } */
int long signed long long *x705; /* { dg-error "" "int long signed long long" } */
int long signed long float *x706; /* { dg-error "" "int long signed long float" } */
int long signed long double *x707; /* { dg-error "" "int long signed long double" } */
int long signed long signed *x708; /* { dg-error "" "int long signed long signed" } */
int long signed long unsigned *x709; /* { dg-error "" "int long signed long unsigned" } */
int long signed long _Bool *x710; /* { dg-error "" "int long signed long _Bool" } */
int long unsigned long void *x711; /* { dg-error "" "int long unsigned long void" } */
int long unsigned long char *x712; /* { dg-error "" "int long unsigned long char" } */
int long unsigned long short *x713; /* { dg-error "" "int long unsigned long short" } */
int long unsigned long int *x714; /* { dg-error "" "int long unsigned long int" } */
int long unsigned long long *x715; /* { dg-error "" "int long unsigned long long" } */
int long unsigned long float *x716; /* { dg-error "" "int long unsigned long float" } */
int long unsigned long double *x717; /* { dg-error "" "int long unsigned long double" } */
int long unsigned long signed *x718; /* { dg-error "" "int long unsigned long signed" } */
int long unsigned long unsigned *x719; /* { dg-error "" "int long unsigned long unsigned" } */
int long unsigned long _Bool *x720; /* { dg-error "" "int long unsigned long _Bool" } */
int signed long long void *x721; /* { dg-error "" "int signed long long void" } */
int signed long long char *x722; /* { dg-error "" "int signed long long char" } */
int signed long long short *x723; /* { dg-error "" "int signed long long short" } */
int signed long long int *x724; /* { dg-error "" "int signed long long int" } */
int signed long long long *x725; /* { dg-error "" "int signed long long long" } */
int signed long long float *x726; /* { dg-error "" "int signed long long float" } */
int signed long long double *x727; /* { dg-error "" "int signed long long double" } */
int signed long long signed *x728; /* { dg-error "" "int signed long long signed" } */
int signed long long unsigned *x729; /* { dg-error "" "int signed long long unsigned" } */
int signed long long _Bool *x730; /* { dg-error "" "int signed long long _Bool" } */
int unsigned long long void *x731; /* { dg-error "" "int unsigned long long void" } */
int unsigned long long char *x732; /* { dg-error "" "int unsigned long long char" } */
int unsigned long long short *x733; /* { dg-error "" "int unsigned long long short" } */
int unsigned long long int *x734; /* { dg-error "" "int unsigned long long int" } */
int unsigned long long long *x735; /* { dg-error "" "int unsigned long long long" } */
int unsigned long long float *x736; /* { dg-error "" "int unsigned long long float" } */
int unsigned long long double *x737; /* { dg-error "" "int unsigned long long double" } */
int unsigned long long signed *x738; /* { dg-error "" "int unsigned long long signed" } */
int unsigned long long unsigned *x739; /* { dg-error "" "int unsigned long long unsigned" } */
int unsigned long long _Bool *x740; /* { dg-error "" "int unsigned long long _Bool" } */
long int long signed void *x741; /* { dg-error "" "long int long signed void" } */
long int long signed char *x742; /* { dg-error "" "long int long signed char" } */
long int long signed short *x743; /* { dg-error "" "long int long signed short" } */
long int long signed int *x744; /* { dg-error "" "long int long signed int" } */
long int long signed long *x745; /* { dg-error "" "long int long signed long" } */
long int long signed float *x746; /* { dg-error "" "long int long signed float" } */
long int long signed double *x747; /* { dg-error "" "long int long signed double" } */
long int long signed signed *x748; /* { dg-error "" "long int long signed signed" } */
long int long signed unsigned *x749; /* { dg-error "" "long int long signed unsigned" } */
long int long signed _Bool *x750; /* { dg-error "" "long int long signed _Bool" } */
long int long unsigned void *x751; /* { dg-error "" "long int long unsigned void" } */
long int long unsigned char *x752; /* { dg-error "" "long int long unsigned char" } */
long int long unsigned short *x753; /* { dg-error "" "long int long unsigned short" } */
long int long unsigned int *x754; /* { dg-error "" "long int long unsigned int" } */
long int long unsigned long *x755; /* { dg-error "" "long int long unsigned long" } */
long int long unsigned float *x756; /* { dg-error "" "long int long unsigned float" } */
long int long unsigned double *x757; /* { dg-error "" "long int long unsigned double" } */
long int long unsigned signed *x758; /* { dg-error "" "long int long unsigned signed" } */
long int long unsigned unsigned *x759; /* { dg-error "" "long int long unsigned unsigned" } */
long int long unsigned _Bool *x760; /* { dg-error "" "long int long unsigned _Bool" } */
long int signed long void *x761; /* { dg-error "" "long int signed long void" } */
long int signed long char *x762; /* { dg-error "" "long int signed long char" } */
long int signed long short *x763; /* { dg-error "" "long int signed long short" } */
long int signed long int *x764; /* { dg-error "" "long int signed long int" } */
long int signed long long *x765; /* { dg-error "" "long int signed long long" } */
long int signed long float *x766; /* { dg-error "" "long int signed long float" } */
long int signed long double *x767; /* { dg-error "" "long int signed long double" } */
long int signed long signed *x768; /* { dg-error "" "long int signed long signed" } */
long int signed long unsigned *x769; /* { dg-error "" "long int signed long unsigned" } */
long int signed long _Bool *x770; /* { dg-error "" "long int signed long _Bool" } */
long int unsigned long void *x771; /* { dg-error "" "long int unsigned long void" } */
long int unsigned long char *x772; /* { dg-error "" "long int unsigned long char" } */
long int unsigned long short *x773; /* { dg-error "" "long int unsigned long short" } */
long int unsigned long int *x774; /* { dg-error "" "long int unsigned long int" } */
long int unsigned long long *x775; /* { dg-error "" "long int unsigned long long" } */
long int unsigned long float *x776; /* { dg-error "" "long int unsigned long float" } */
long int unsigned long double *x777; /* { dg-error "" "long int unsigned long double" } */
long int unsigned long signed *x778; /* { dg-error "" "long int unsigned long signed" } */
long int unsigned long unsigned *x779; /* { dg-error "" "long int unsigned long unsigned" } */
long int unsigned long _Bool *x780; /* { dg-error "" "long int unsigned long _Bool" } */
long long int signed void *x781; /* { dg-error "" "long long int signed void" } */
long long int signed char *x782; /* { dg-error "" "long long int signed char" } */
long long int signed short *x783; /* { dg-error "" "long long int signed short" } */
long long int signed int *x784; /* { dg-error "" "long long int signed int" } */
long long int signed long *x785; /* { dg-error "" "long long int signed long" } */
long long int signed float *x786; /* { dg-error "" "long long int signed float" } */
long long int signed double *x787; /* { dg-error "" "long long int signed double" } */
long long int signed signed *x788; /* { dg-error "" "long long int signed signed" } */
long long int signed unsigned *x789; /* { dg-error "" "long long int signed unsigned" } */
long long int signed _Bool *x790; /* { dg-error "" "long long int signed _Bool" } */
long long int unsigned void *x791; /* { dg-error "" "long long int unsigned void" } */
long long int unsigned char *x792; /* { dg-error "" "long long int unsigned char" } */
long long int unsigned short *x793; /* { dg-error "" "long long int unsigned short" } */
long long int unsigned int *x794; /* { dg-error "" "long long int unsigned int" } */
long long int unsigned long *x795; /* { dg-error "" "long long int unsigned long" } */
long long int unsigned float *x796; /* { dg-error "" "long long int unsigned float" } */
long long int unsigned double *x797; /* { dg-error "" "long long int unsigned double" } */
long long int unsigned signed *x798; /* { dg-error "" "long long int unsigned signed" } */
long long int unsigned unsigned *x799; /* { dg-error "" "long long int unsigned unsigned" } */
long long int unsigned _Bool *x800; /* { dg-error "" "long long int unsigned _Bool" } */
long long signed int void *x801; /* { dg-error "" "long long signed int void" } */
long long signed int char *x802; /* { dg-error "" "long long signed int char" } */
long long signed int short *x803; /* { dg-error "" "long long signed int short" } */
long long signed int int *x804; /* { dg-error "" "long long signed int int" } */
long long signed int long *x805; /* { dg-error "" "long long signed int long" } */
long long signed int float *x806; /* { dg-error "" "long long signed int float" } */
long long signed int double *x807; /* { dg-error "" "long long signed int double" } */
long long signed int signed *x808; /* { dg-error "" "long long signed int signed" } */
long long signed int unsigned *x809; /* { dg-error "" "long long signed int unsigned" } */
long long signed int _Bool *x810; /* { dg-error "" "long long signed int _Bool" } */
long long unsigned int void *x811; /* { dg-error "" "long long unsigned int void" } */
long long unsigned int char *x812; /* { dg-error "" "long long unsigned int char" } */
long long unsigned int short *x813; /* { dg-error "" "long long unsigned int short" } */
long long unsigned int int *x814; /* { dg-error "" "long long unsigned int int" } */
long long unsigned int long *x815; /* { dg-error "" "long long unsigned int long" } */
long long unsigned int float *x816; /* { dg-error "" "long long unsigned int float" } */
long long unsigned int double *x817; /* { dg-error "" "long long unsigned int double" } */
long long unsigned int signed *x818; /* { dg-error "" "long long unsigned int signed" } */
long long unsigned int unsigned *x819; /* { dg-error "" "long long unsigned int unsigned" } */
long long unsigned int _Bool *x820; /* { dg-error "" "long long unsigned int _Bool" } */
long signed int long void *x821; /* { dg-error "" "long signed int long void" } */
long signed int long char *x822; /* { dg-error "" "long signed int long char" } */
long signed int long short *x823; /* { dg-error "" "long signed int long short" } */
long signed int long int *x824; /* { dg-error "" "long signed int long int" } */
long signed int long long *x825; /* { dg-error "" "long signed int long long" } */
long signed int long float *x826; /* { dg-error "" "long signed int long float" } */
long signed int long double *x827; /* { dg-error "" "long signed int long double" } */
long signed int long signed *x828; /* { dg-error "" "long signed int long signed" } */
long signed int long unsigned *x829; /* { dg-error "" "long signed int long unsigned" } */
long signed int long _Bool *x830; /* { dg-error "" "long signed int long _Bool" } */
long signed long int void *x831; /* { dg-error "" "long signed long int void" } */
long signed long int char *x832; /* { dg-error "" "long signed long int char" } */
long signed long int short *x833; /* { dg-error "" "long signed long int short" } */
long signed long int int *x834; /* { dg-error "" "long signed long int int" } */
long signed long int long *x835; /* { dg-error "" "long signed long int long" } */
long signed long int float *x836; /* { dg-error "" "long signed long int float" } */
long signed long int double *x837; /* { dg-error "" "long signed long int double" } */
long signed long int signed *x838; /* { dg-error "" "long signed long int signed" } */
long signed long int unsigned *x839; /* { dg-error "" "long signed long int unsigned" } */
long signed long int _Bool *x840; /* { dg-error "" "long signed long int _Bool" } */
long unsigned int long void *x841; /* { dg-error "" "long unsigned int long void" } */
long unsigned int long char *x842; /* { dg-error "" "long unsigned int long char" } */
long unsigned int long short *x843; /* { dg-error "" "long unsigned int long short" } */
long unsigned int long int *x844; /* { dg-error "" "long unsigned int long int" } */
long unsigned int long long *x845; /* { dg-error "" "long unsigned int long long" } */
long unsigned int long float *x846; /* { dg-error "" "long unsigned int long float" } */
long unsigned int long double *x847; /* { dg-error "" "long unsigned int long double" } */
long unsigned int long signed *x848; /* { dg-error "" "long unsigned int long signed" } */
long unsigned int long unsigned *x849; /* { dg-error "" "long unsigned int long unsigned" } */
long unsigned int long _Bool *x850; /* { dg-error "" "long unsigned int long _Bool" } */
long unsigned long int void *x851; /* { dg-error "" "long unsigned long int void" } */
long unsigned long int char *x852; /* { dg-error "" "long unsigned long int char" } */
long unsigned long int short *x853; /* { dg-error "" "long unsigned long int short" } */
long unsigned long int int *x854; /* { dg-error "" "long unsigned long int int" } */
long unsigned long int long *x855; /* { dg-error "" "long unsigned long int long" } */
long unsigned long int float *x856; /* { dg-error "" "long unsigned long int float" } */
long unsigned long int double *x857; /* { dg-error "" "long unsigned long int double" } */
long unsigned long int signed *x858; /* { dg-error "" "long unsigned long int signed" } */
long unsigned long int unsigned *x859; /* { dg-error "" "long unsigned long int unsigned" } */
long unsigned long int _Bool *x860; /* { dg-error "" "long unsigned long int _Bool" } */
signed int long long void *x861; /* { dg-error "" "signed int long long void" } */
signed int long long char *x862; /* { dg-error "" "signed int long long char" } */
signed int long long short *x863; /* { dg-error "" "signed int long long short" } */
signed int long long int *x864; /* { dg-error "" "signed int long long int" } */
signed int long long long *x865; /* { dg-error "" "signed int long long long" } */
signed int long long float *x866; /* { dg-error "" "signed int long long float" } */
signed int long long double *x867; /* { dg-error "" "signed int long long double" } */
signed int long long signed *x868; /* { dg-error "" "signed int long long signed" } */
signed int long long unsigned *x869; /* { dg-error "" "signed int long long unsigned" } */
signed int long long _Bool *x870; /* { dg-error "" "signed int long long _Bool" } */
signed long int long void *x871; /* { dg-error "" "signed long int long void" } */
signed long int long char *x872; /* { dg-error "" "signed long int long char" } */
signed long int long short *x873; /* { dg-error "" "signed long int long short" } */
signed long int long int *x874; /* { dg-error "" "signed long int long int" } */
signed long int long long *x875; /* { dg-error "" "signed long int long long" } */
signed long int long float *x876; /* { dg-error "" "signed long int long float" } */
signed long int long double *x877; /* { dg-error "" "signed long int long double" } */
signed long int long signed *x878; /* { dg-error "" "signed long int long signed" } */
signed long int long unsigned *x879; /* { dg-error "" "signed long int long unsigned" } */
signed long int long _Bool *x880; /* { dg-error "" "signed long int long _Bool" } */
signed long long int void *x881; /* { dg-error "" "signed long long int void" } */
signed long long int char *x882; /* { dg-error "" "signed long long int char" } */
signed long long int short *x883; /* { dg-error "" "signed long long int short" } */
signed long long int int *x884; /* { dg-error "" "signed long long int int" } */
signed long long int long *x885; /* { dg-error "" "signed long long int long" } */
signed long long int float *x886; /* { dg-error "" "signed long long int float" } */
signed long long int double *x887; /* { dg-error "" "signed long long int double" } */
signed long long int signed *x888; /* { dg-error "" "signed long long int signed" } */
signed long long int unsigned *x889; /* { dg-error "" "signed long long int unsigned" } */
signed long long int _Bool *x890; /* { dg-error "" "signed long long int _Bool" } */
unsigned int long long void *x891; /* { dg-error "" "unsigned int long long void" } */
unsigned int long long char *x892; /* { dg-error "" "unsigned int long long char" } */
unsigned int long long short *x893; /* { dg-error "" "unsigned int long long short" } */
unsigned int long long int *x894; /* { dg-error "" "unsigned int long long int" } */
unsigned int long long long *x895; /* { dg-error "" "unsigned int long long long" } */
unsigned int long long float *x896; /* { dg-error "" "unsigned int long long float" } */
unsigned int long long double *x897; /* { dg-error "" "unsigned int long long double" } */
unsigned int long long signed *x898; /* { dg-error "" "unsigned int long long signed" } */
unsigned int long long unsigned *x899; /* { dg-error "" "unsigned int long long unsigned" } */
unsigned int long long _Bool *x900; /* { dg-error "" "unsigned int long long _Bool" } */
unsigned long int long void *x901; /* { dg-error "" "unsigned long int long void" } */
unsigned long int long char *x902; /* { dg-error "" "unsigned long int long char" } */
unsigned long int long short *x903; /* { dg-error "" "unsigned long int long short" } */
unsigned long int long int *x904; /* { dg-error "" "unsigned long int long int" } */
unsigned long int long long *x905; /* { dg-error "" "unsigned long int long long" } */
unsigned long int long float *x906; /* { dg-error "" "unsigned long int long float" } */
unsigned long int long double *x907; /* { dg-error "" "unsigned long int long double" } */
unsigned long int long signed *x908; /* { dg-error "" "unsigned long int long signed" } */
unsigned long int long unsigned *x909; /* { dg-error "" "unsigned long int long unsigned" } */
unsigned long int long _Bool *x910; /* { dg-error "" "unsigned long int long _Bool" } */
unsigned long long int void *x911; /* { dg-error "" "unsigned long long int void" } */
unsigned long long int char *x912; /* { dg-error "" "unsigned long long int char" } */
unsigned long long int short *x913; /* { dg-error "" "unsigned long long int short" } */
unsigned long long int int *x914; /* { dg-error "" "unsigned long long int int" } */
unsigned long long int long *x915; /* { dg-error "" "unsigned long long int long" } */
unsigned long long int float *x916; /* { dg-error "" "unsigned long long int float" } */
unsigned long long int double *x917; /* { dg-error "" "unsigned long long int double" } */
unsigned long long int signed *x918; /* { dg-error "" "unsigned long long int signed" } */
unsigned long long int unsigned *x919; /* { dg-error "" "unsigned long long int unsigned" } */
unsigned long long int _Bool *x920; /* { dg-error "" "unsigned long long int _Bool" } */
