/* Test for valid and invalid combinations of type specifiers using __int128.
   */
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do compile { target int128 } } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "" { target c++ } } */

typedef char type;
__int128 *x0;
void __int128 *x1; /* { dg-error "" } */
char __int128 *x2; /* { dg-error "" } */
short __int128 *x3; /* { dg-error "" } */
int __int128 *x4; /* { dg-error "" } */
__int128 void *x5; /* { dg-error "" } */
__int128 char *x6; /* { dg-error "" } */
__int128 short *x7; /* { dg-error "" } */
__int128 int *x8; /* { dg-error "" } */
__int128 __int128 *x9; /* { dg-error "" } */
__int128 long *x10; /* { dg-error "" } */
__int128 float *x11; /* { dg-error "" } */
__int128 double *x12; /* { dg-error "" } */
__int128 signed *x13;
__int128 unsigned *x14;
__int128 _Bool *x15; /* { dg-error "" } */
__int128 _Complex *x16;
long __int128 *x17; /* { dg-error "" } */
float __int128 *x18; /* { dg-error "" } */
double __int128 *x19; /* { dg-error "" } */
signed __int128 *x20;
unsigned __int128 *x21;
_Bool __int128 *x22; /* { dg-error "" } */
_Complex __int128 *x23;
type __int128 *x24; /* { dg-error "" } */
char signed __int128 *x25; /* { dg-error "" } */
char unsigned __int128 *x26; /* { dg-error "" } */
char _Complex __int128 *x27; /* { dg-error "" } */
short int __int128 *x28; /* { dg-error "" } */
short signed __int128 *x29; /* { dg-error "" } */
short unsigned __int128 *x30; /* { dg-error "" } */
short _Complex __int128 *x31; /* { dg-error "" } */
int short __int128 *x32; /* { dg-error "" } */
int long __int128 *x33; /* { dg-error "" } */
int signed __int128 *x34; /* { dg-error "" } */
int unsigned __int128 *x35; /* { dg-error "" } */
int _Complex __int128 *x36; /* { dg-error "" } */
__int128 signed void *x37; /* { dg-error "" } */
__int128 signed char *x38; /* { dg-error ""  } */
__int128 signed short *x39; /* { dg-error "" } */
__int128 signed int *x40; /* { dg-error "" } */
__int128 signed __int128 *x41; /* { dg-error "" } */
__int128 signed long *x42; /* { dg-error "" } */
__int128 signed float *x43; /* { dg-error "" } */
__int128 signed double *x44; /* { dg-error "" } */
__int128 signed signed *x45; /* { dg-error "" } */
__int128 signed unsigned *x46; /* { dg-error "" } */
__int128 signed _Bool *x47; /* { dg-error "" } */
__int128 signed _Complex *x48;
__int128 unsigned void *x49; /* { dg-error "" } */
__int128 unsigned char *x50; /* { dg-error "" } */
__int128 unsigned short *x51; /* { dg-error "" } */
__int128 unsigned int *x52; /* { dg-error "" } */
__int128 unsigned __int128 *x53; /* { dg-error "" } */
__int128 unsigned long *x54; /* { dg-error "" } */
__int128 unsigned float *x55; /* { dg-error "" } */
__int128 unsigned double *x56; /* { dg-error "" } */
__int128 unsigned signed *x57; /* { dg-error "" } */
__int128 unsigned unsigned *x58; /* { dg-error "" } */
__int128 unsigned _Bool *x59; /* { dg-error "" } */
__int128 unsigned _Complex *x60;
__int128 _Complex void *x61; /* { dg-error "" } */
__int128 _Complex char *x62; /* { dg-error "" } */
__int128 _Complex short *x63; /* { dg-error "" } */
__int128 _Complex int *x64; /* { dg-error "" } */
__int128 _Complex __int128 *x65; /* { dg-error "" } */
__int128 _Complex long *x66; /* { dg-error "" } */
__int128 _Complex float *x67; /* { dg-error "" } */
__int128 _Complex double *x68; /* { dg-error "" } */
__int128 _Complex signed *x69;
__int128 _Complex unsigned *x70;
__int128 _Complex _Bool *x71; /* { dg-error "" } */
__int128 _Complex _Complex *x72; /* { dg-error "" } */
long int __int128 *x73; /* { dg-error "" } */
long long __int128 *x74; /* { dg-error "" } */
long double __int128 *x75; /* { dg-error "" } */
long signed __int128 *x76; /* { dg-error "" } */
long unsigned __int128 *x77; /* { dg-error "" } */
long _Complex __int128 *x78; /* { dg-error "" } */
float _Complex __int128 *x79; /* { dg-error "" } */
double long __int128 *x80; /* { dg-error "" } */
double _Complex __int128 *x81; /* { dg-error "" } */
signed char __int128 *x82; /* { dg-error "" } */
signed short __int128 *x83; /* { dg-error "" } */
signed int __int128 *x84; /* { dg-error "" } */
signed __int128 void *x85; /* { dg-error "" } */
signed __int128 char *x86; /* { dg-error "" } */
signed __int128 short *x87; /* { dg-error "" } */
signed __int128 int *x88; /* { dg-error "" } */
signed __int128 __int128 *x89; /* { dg-error "" } */
signed __int128 long *x90; /* { dg-error "" } */
signed __int128 float *x91; /* { dg-error "" } */
signed __int128 double *x92; /* { dg-error "" } */
signed __int128 signed *x93; /* { dg-error "" } */
signed __int128 unsigned *x94; /* { dg-error "" } */
signed __int128 _Bool *x95; /* { dg-error "" } */
signed __int128 _Complex *x96;
signed long __int128 *x97; /* { dg-error "" } */
signed _Complex __int128 *x98;
unsigned char __int128 *x99; /* { dg-error "" } */
unsigned short __int128 *x100; /* { dg-error "" } */
unsigned int __int128 *x101; /* { dg-error "" } */
unsigned __int128 void *x102; /* { dg-error "" } */
unsigned __int128 char *x103; /* { dg-error "" } */
unsigned __int128 short *x104; /* { dg-error "" } */
unsigned __int128 int *x105; /* { dg-error "" } */
unsigned __int128 __int128 *x106; /* { dg-error "" } */
unsigned __int128 long *x107; /* { dg-error "" } */
unsigned __int128 float *x108; /* { dg-error "" } */
unsigned __int128 double *x109; /* { dg-error "" } */
unsigned __int128 signed *x110; /* { dg-error "" } */
unsigned __int128 unsigned *x111; /* { dg-error "" } */
unsigned __int128 _Bool *x112; /* { dg-error "" } */
unsigned __int128 _Complex *x113;
unsigned long __int128 *x114; /* { dg-error "" } */
unsigned _Complex __int128 *x115;
_Complex char __int128 *x116; /* { dg-error "" } */
_Complex short __int128 *x117; /* { dg-error "" } */
_Complex int __int128 *x118; /* { dg-error "" } */
_Complex __int128 void *x119; /* { dg-error "" } */
_Complex __int128 char *x120; /* { dg-error "" } */
_Complex __int128 short *x121; /* { dg-error "" } */
_Complex __int128 int *x122; /* { dg-error "" } */
_Complex __int128 __int128 *x123; /* { dg-error "" } */
_Complex __int128 long *x124; /* { dg-error "" } */
_Complex __int128 float *x125; /* { dg-error "" } */
_Complex __int128 double *x126; /* { dg-error "" } */
_Complex __int128 signed *x127;
_Complex __int128 unsigned *x128;
_Complex __int128 _Bool *x129; /* { dg-error "" } */
_Complex __int128 _Complex *x130; /* { dg-error "" } */
_Complex long __int128 *x131; /* { dg-error "" } */
_Complex float __int128 *x132; /* { dg-error "" } */
_Complex double __int128 *x133; /* { dg-error "" } */
_Complex signed __int128 *x134;
_Complex unsigned __int128 *x135;
char signed _Complex __int128 *x136; /* { dg-error "" } */
char unsigned _Complex __int128 *x137; /* { dg-error "" } */
char _Complex signed __int128 *x138; /* { dg-error "" } */
char _Complex unsigned __int128 *x139; /* { dg-error "" } */
short int signed __int128 *x140; /* { dg-error "" } */
short int unsigned __int128 *x141; /* { dg-error "" } */
short int _Complex __int128 *x142; /* { dg-error "" } */
short signed int __int128 *x143; /* { dg-error "" } */
short signed _Complex __int128 *x144; /* { dg-error "" } */
short unsigned int __int128 *x145; /* { dg-error "" } */
short unsigned _Complex __int128 *x146; /* { dg-error "" } */
short _Complex int __int128 *x147; /* { dg-error "" } */
short _Complex signed __int128 *x148; /* { dg-error "" } */
short _Complex unsigned __int128 *x149; /* { dg-error "" } */
int short signed __int128 *x150; /* { dg-error "" } */
int short unsigned __int128 *x151; /* { dg-error "" } */
int short _Complex __int128 *x152; /* { dg-error "" } */
int long long __int128 *x153; /* { dg-error "" } */
int long signed __int128 *x154; /* { dg-error "" } */
int long unsigned __int128 *x155; /* { dg-error "" } */
int long _Complex __int128 *x156; /* { dg-error "" } */
int signed short __int128 *x157; /* { dg-error "" } */
int signed long __int128 *x158; /* { dg-error "" } */
int signed _Complex __int128 *x159; /* { dg-error "" } */
int unsigned short __int128 *x160; /* { dg-error "" } */
int unsigned long __int128 *x161; /* { dg-error "" } */
int unsigned _Complex __int128 *x162; /* { dg-error "" } */
int _Complex short __int128 *x163; /* { dg-error "" } */
int _Complex long __int128 *x164; /* { dg-error "" } */
int _Complex signed __int128 *x165; /* { dg-error "" } */
int _Complex unsigned __int128 *x166; /* { dg-error "" } */
__int128 signed _Complex void *x167; /* { dg-error "" } */
__int128 signed _Complex char *x168; /* { dg-error "" } */
__int128 signed _Complex short *x169; /* { dg-error "" } */
__int128 signed _Complex int *x170; /* { dg-error "" } */
__int128 signed _Complex __int128 *x171; /* { dg-error "" } */
__int128 signed _Complex long *x172; /* { dg-error "" } */
__int128 signed _Complex float *x173; /* { dg-error "" } */
__int128 signed _Complex double *x174; /* { dg-error "" } */
__int128 signed _Complex signed *x175; /* { dg-error "" } */
__int128 signed _Complex unsigned *x176; /* { dg-error "" } */
__int128 signed _Complex _Bool *x177; /* { dg-error "" } */
__int128 signed _Complex _Complex *x178; /* { dg-error "" } */
__int128 unsigned _Complex void *x179; /* { dg-error "" } */
__int128 unsigned _Complex char *x180; /* { dg-error "" } */
__int128 unsigned _Complex short *x181; /* { dg-error "" } */
__int128 unsigned _Complex int *x182; /* { dg-error "" } */
__int128 unsigned _Complex __int128 *x183; /* { dg-error "" } */
__int128 unsigned _Complex long *x184; /* { dg-error "" } */
__int128 unsigned _Complex float *x185; /* { dg-error "" } */
__int128 unsigned _Complex double *x186; /* { dg-error "" } */
__int128 unsigned _Complex signed *x187; /* { dg-error "" } */
__int128 unsigned _Complex unsigned *x188; /* { dg-error "" } */
__int128 unsigned _Complex _Bool *x189; /* { dg-error "" } */
__int128 unsigned _Complex _Complex *x190; /* { dg-error "" } */
__int128 _Complex signed void *x191; /* { dg-error "" } */
__int128 _Complex signed char *x192; /* { dg-error "" } */
__int128 _Complex signed short *x193; /* { dg-error "" } */
__int128 _Complex signed int *x194; /* { dg-error "" } */
__int128 _Complex signed __int128 *x195; /* { dg-error "" } */
__int128 _Complex signed long *x196; /* { dg-error "" } */
__int128 _Complex signed float *x197; /* { dg-error "" } */
__int128 _Complex signed double *x198; /* { dg-error "" } */
__int128 _Complex signed signed *x199; /* { dg-error "" } */
__int128 _Complex signed unsigned *x200; /* { dg-error "" } */
__int128 _Complex signed _Bool *x201; /* { dg-error "" } */
__int128 _Complex signed _Complex *x202; /* { dg-error "" } */
__int128 _Complex unsigned void *x203; /* { dg-error "" } */
__int128 _Complex unsigned char *x204; /* { dg-error "" } */
__int128 _Complex unsigned short *x205; /* { dg-error "" } */
__int128 _Complex unsigned int *x206; /* { dg-error "" } */
__int128 _Complex unsigned __int128 *x207; /* { dg-error "" } */
__int128 _Complex unsigned long *x208; /* { dg-error "" } */
__int128 _Complex unsigned float *x209; /* { dg-error "" } */
__int128 _Complex unsigned double *x210; /* { dg-error "" } */
__int128 _Complex unsigned signed *x211; /* { dg-error "" } */
__int128 _Complex unsigned unsigned *x212; /* { dg-error "" } */
__int128 _Complex unsigned _Bool *x213; /* { dg-error "" } */
__int128 _Complex unsigned _Complex *x214; /* { dg-error "" } */
long int long __int128 *x215; /* { dg-error "" } */
long int signed __int128 *x216; /* { dg-error "" } */
long int unsigned __int128 *x217; /* { dg-error "" } */
long int _Complex __int128 *x218; /* { dg-error "" } */
long long int __int128 *x219; /* { dg-error "" } */
long long signed __int128 *x220; /* { dg-error "" } */
long long unsigned __int128 *x221; /* { dg-error "" } */
long long _Complex __int128 *x222; /* { dg-error "" } */
long double _Complex __int128 *x223; /* { dg-error "" } */
long signed int __int128 *x224; /* { dg-error "" } */
long signed long __int128 *x225; /* { dg-error "" } */
long signed _Complex __int128 *x226; /* { dg-error "" } */
long unsigned int __int128 *x227; /* { dg-error "" } */
long unsigned long __int128 *x228; /* { dg-error "" } */
long unsigned _Complex __int128 *x229; /* { dg-error "" } */
long _Complex int __int128 *x230; /* { dg-error "" } */
long _Complex long __int128 *x231; /* { dg-error "" } */
long _Complex double __int128 *x232; /* { dg-error "" } */
long _Complex signed __int128 *x233; /* { dg-error "" } */
long _Complex unsigned __int128 *x234; /* { dg-error "" } */
double long _Complex __int128 *x235; /* { dg-error "" } */
double _Complex long __int128 *x236; /* { dg-error "" } */
signed char _Complex __int128 *x237; /* { dg-error "" } */
signed short int __int128 *x238; /* { dg-error "" } */
signed short _Complex __int128 *x239; /* { dg-error "" } */
signed int short __int128 *x240; /* { dg-error "" } */
signed int long __int128 *x241; /* { dg-error "" } */
signed int _Complex __int128 *x242; /* { dg-error "" } */
signed __int128 _Complex void *x243; /* { dg-error "" } */
signed __int128 _Complex char *x244; /* { dg-error "" } */
signed __int128 _Complex short *x245; /* { dg-error "" } */
signed __int128 _Complex int *x246; /* { dg-error "" } */
signed __int128 _Complex __int128 *x247; /* { dg-error "" } */
signed __int128 _Complex long *x248; /* { dg-error "" } */
signed __int128 _Complex float *x249; /* { dg-error "" } */
signed __int128 _Complex double *x250; /* { dg-error "" } */
signed __int128 _Complex signed *x251; /* { dg-error "" } */
signed __int128 _Complex unsigned *x252; /* { dg-error "" } */
signed __int128 _Complex _Bool *x253; /* { dg-error "" } */
signed __int128 _Complex _Complex *x254; /* { dg-error "" } */
signed long int __int128 *x255; /* { dg-error "" } */
signed long long __int128 *x256; /* { dg-error "" } */
signed long _Complex __int128 *x257; /* { dg-error "" } */
signed _Complex char __int128 *x258; /* { dg-error "" } */
signed _Complex short __int128 *x259; /* { dg-error "" } */
signed _Complex int __int128 *x260; /* { dg-error "" } */
signed _Complex __int128 void *x261; /* { dg-error "" } */
signed _Complex __int128 char *x262; /* { dg-error "" } */
signed _Complex __int128 short *x263; /* { dg-error "" } */
signed _Complex __int128 int *x264; /* { dg-error "" } */
signed _Complex __int128 __int128 *x265; /* { dg-error "" } */
signed _Complex __int128 long *x266; /* { dg-error "" } */
signed _Complex __int128 float *x267; /* { dg-error "" } */
signed _Complex __int128 double *x268; /* { dg-error "" } */
signed _Complex __int128 signed *x269; /* { dg-error "" } */
signed _Complex __int128 unsigned *x270; /* { dg-error "" } */
signed _Complex __int128 _Bool *x271; /* { dg-error "" } */
signed _Complex __int128 _Complex *x272; /* { dg-error "" } */
signed _Complex long __int128 *x273; /* { dg-error "" } */
unsigned char _Complex __int128 *x274; /* { dg-error "" } */
unsigned short int __int128 *x275; /* { dg-error "" } */
unsigned short _Complex __int128 *x276; /* { dg-error "" } */
unsigned int short __int128 *x277; /* { dg-error "" } */
unsigned int long __int128 *x278; /* { dg-error "" } */
unsigned int _Complex __int128 *x279; /* { dg-error "" } */
unsigned __int128 _Complex void *x280; /* { dg-error "" } */
unsigned __int128 _Complex char *x281; /* { dg-error "" } */
unsigned __int128 _Complex short *x282; /* { dg-error "" } */
unsigned __int128 _Complex int *x283; /* { dg-error "" } */
unsigned __int128 _Complex __int128 *x284; /* { dg-error "" } */
unsigned __int128 _Complex long *x285; /* { dg-error "" } */
unsigned __int128 _Complex float *x286; /* { dg-error "" } */
unsigned __int128 _Complex double *x287; /* { dg-error "" } */
unsigned __int128 _Complex signed *x288; /* { dg-error "" } */
unsigned __int128 _Complex unsigned *x289; /* { dg-error "" } */
unsigned __int128 _Complex _Bool *x290; /* { dg-error "" } */
unsigned __int128 _Complex _Complex *x291; /* { dg-error "" } */
unsigned long int __int128 *x292; /* { dg-error "" } */
unsigned long long __int128 *x293; /* { dg-error "" } */
unsigned long _Complex __int128 *x294; /* { dg-error "" } */
unsigned _Complex char __int128 *x295; /* { dg-error "" } */
unsigned _Complex short __int128 *x296; /* { dg-error "" } */
unsigned _Complex int __int128 *x297; /* { dg-error "" } */
unsigned _Complex __int128 void *x298; /* { dg-error "" } */
unsigned _Complex __int128 char *x299; /* { dg-error "" } */
unsigned _Complex __int128 short *x300; /* { dg-error "" } */
unsigned _Complex __int128 int *x301; /* { dg-error "" } */
unsigned _Complex __int128 __int128 *x302; /* { dg-error "" } */
unsigned _Complex __int128 long *x303; /* { dg-error "" } */
unsigned _Complex __int128 float *x304; /* { dg-error "" } */
unsigned _Complex __int128 double *x305; /* { dg-error "" } */
unsigned _Complex __int128 signed *x306; /* { dg-error "" } */
unsigned _Complex __int128 unsigned *x307; /* { dg-error "" } */
unsigned _Complex __int128 _Bool *x308; /* { dg-error "" } */
unsigned _Complex __int128 _Complex *x309; /* { dg-error "" } */
unsigned _Complex long __int128 *x310; /* { dg-error "" } */
_Complex char signed __int128 *x311; /* { dg-error "" } */
_Complex char unsigned __int128 *x312; /* { dg-error "" } */
_Complex short int __int128 *x313; /* { dg-error "" } */
_Complex short signed __int128 *x314; /* { dg-error "" } */
_Complex short unsigned __int128 *x315; /* { dg-error "" } */
_Complex int short __int128 *x316; /* { dg-error "" } */
_Complex int long __int128 *x317; /* { dg-error "" } */
_Complex int signed __int128 *x318; /* { dg-error "" } */
_Complex int unsigned __int128 *x319; /* { dg-error "" } */
_Complex __int128 signed void *x320; /* { dg-error "" } */
_Complex __int128 signed char *x321; /* { dg-error "" } */
_Complex __int128 signed short *x322; /* { dg-error "" } */
_Complex __int128 signed int *x323; /* { dg-error "" } */
_Complex __int128 signed __int128 *x324; /* { dg-error "" } */
_Complex __int128 signed long *x325; /* { dg-error "" } */
_Complex __int128 signed float *x326; /* { dg-error "" } */
_Complex __int128 signed double *x327; /* { dg-error "" } */
_Complex __int128 signed signed *x328; /* { dg-error "" } */
_Complex __int128 signed unsigned *x329; /* { dg-error "" } */
_Complex __int128 signed _Bool *x330; /* { dg-error "" } */
_Complex __int128 signed _Complex *x331; /* { dg-error "" } */
_Complex __int128 unsigned void *x332; /* { dg-error "" } */
_Complex __int128 unsigned char *x333; /* { dg-error "" } */
_Complex __int128 unsigned short *x334; /* { dg-error "" } */
_Complex __int128 unsigned int *x335; /* { dg-error "" } */
_Complex __int128 unsigned __int128 *x336; /* { dg-error "" } */
_Complex __int128 unsigned long *x337; /* { dg-error "" } */
_Complex __int128 unsigned float *x338; /* { dg-error "" } */
_Complex __int128 unsigned double *x339; /* { dg-error "" } */
_Complex __int128 unsigned signed *x340; /* { dg-error "" } */
_Complex __int128 unsigned unsigned *x341; /* { dg-error "" } */
_Complex __int128 unsigned _Bool *x342; /* { dg-error "" } */
_Complex __int128 unsigned _Complex *x343; /* { dg-error "" } */
_Complex long int __int128 *x344; /* { dg-error "" } */
_Complex long long __int128 *x345; /* { dg-error "" } */
_Complex long double __int128 *x346; /* { dg-error "" } */
_Complex long signed __int128 *x347; /* { dg-error "" } */
_Complex long unsigned __int128 *x348; /* { dg-error "" } */
_Complex double long __int128 *x349; /* { dg-error "" } */
_Complex signed char __int128 *x350; /* { dg-error "" } */
_Complex signed short __int128 *x351; /* { dg-error "" } */
_Complex signed int __int128 *x352; /* { dg-error "" } */
_Complex signed __int128 void *x353; /* { dg-error "" } */
_Complex signed __int128 char *x354; /* { dg-error "" } */
_Complex signed __int128 short *x355; /* { dg-error "" } */
_Complex signed __int128 int *x356; /* { dg-error "" } */
_Complex signed __int128 __int128 *x357; /* { dg-error "" } */
_Complex signed __int128 long *x358; /* { dg-error "" } */
_Complex signed __int128 float *x359; /* { dg-error "" } */
_Complex signed __int128 double *x360; /* { dg-error "" } */
_Complex signed __int128 signed *x361; /* { dg-error "" } */
_Complex signed __int128 unsigned *x362; /* { dg-error "" } */
_Complex signed __int128 _Bool *x363; /* { dg-error "" } */
_Complex signed __int128 _Complex *x364; /* { dg-error "" } */
_Complex signed long __int128 *x365; /* { dg-error "" } */
_Complex unsigned char __int128 *x366; /* { dg-error "" } */
_Complex unsigned short __int128 *x367; /* { dg-error "" } */
_Complex unsigned int __int128 *x368; /* { dg-error "" } */
_Complex unsigned __int128 void *x369; /* { dg-error "" } */
_Complex unsigned __int128 char *x370; /* { dg-error "" } */
_Complex unsigned __int128 short *x371; /* { dg-error "" } */
_Complex unsigned __int128 int *x372; /* { dg-error "" } */
_Complex unsigned __int128 __int128 *x373; /* { dg-error "" } */
_Complex unsigned __int128 long *x374; /* { dg-error "" } */
_Complex unsigned __int128 float *x375; /* { dg-error "" } */
_Complex unsigned __int128 double *x376; /* { dg-error "" } */
_Complex unsigned __int128 signed *x377; /* { dg-error "" } */
_Complex unsigned __int128 unsigned *x378; /* { dg-error "" } */
_Complex unsigned __int128 _Bool *x379; /* { dg-error "" } */
_Complex unsigned __int128 _Complex *x380; /* { dg-error "" } */
_Complex unsigned long __int128 *x381; /* { dg-error "" } */
short int signed _Complex __int128 *x382; /* { dg-error "" } */
short int unsigned _Complex __int128 *x383; /* { dg-error "" } */
short int _Complex signed __int128 *x384; /* { dg-error "" } */
short int _Complex unsigned __int128 *x385; /* { dg-error "" } */
short signed int _Complex __int128 *x386; /* { dg-error "" } */
short signed _Complex int __int128 *x387; /* { dg-error "" } */
short unsigned int _Complex __int128 *x388; /* { dg-error "" } */
short unsigned _Complex int __int128 *x389; /* { dg-error "" } */
short _Complex int signed __int128 *x390; /* { dg-error "" } */
short _Complex int unsigned __int128 *x391; /* { dg-error "" } */
short _Complex signed int __int128 *x392; /* { dg-error "" } */
short _Complex unsigned int __int128 *x393; /* { dg-error "" } */
int short signed _Complex __int128 *x394; /* { dg-error "" } */
int short unsigned _Complex __int128 *x395; /* { dg-error "" } */
int short _Complex signed __int128 *x396; /* { dg-error "" } */
int short _Complex unsigned __int128 *x397; /* { dg-error "" } */
int long long signed __int128 *x398; /* { dg-error "" } */
int long long unsigned __int128 *x399; /* { dg-error "" } */
int long long _Complex __int128 *x400; /* { dg-error "" } */
int long signed long __int128 *x401; /* { dg-error "" } */
int long signed _Complex __int128 *x402; /* { dg-error "" } */
int long unsigned long __int128 *x403; /* { dg-error "" } */
int long unsigned _Complex __int128 *x404; /* { dg-error "" } */
int long _Complex long __int128 *x405; /* { dg-error "" } */
int long _Complex signed __int128 *x406; /* { dg-error "" } */
int long _Complex unsigned __int128 *x407; /* { dg-error "" } */
int signed short _Complex __int128 *x408; /* { dg-error "" } */
int signed long long __int128 *x409; /* { dg-error "" } */
int signed long _Complex __int128 *x410; /* { dg-error "" } */
int signed _Complex short __int128 *x411; /* { dg-error "" } */
int signed _Complex long __int128 *x412; /* { dg-error "" } */
int unsigned short _Complex __int128 *x413; /* { dg-error "" } */
int unsigned long long __int128 *x414; /* { dg-error "" } */
int unsigned long _Complex __int128 *x415; /* { dg-error "" } */
int unsigned _Complex short __int128 *x416; /* { dg-error "" } */
int unsigned _Complex long __int128 *x417; /* { dg-error "" } */
int _Complex short signed __int128 *x418; /* { dg-error "" } */
int _Complex short unsigned __int128 *x419; /* { dg-error "" } */
int _Complex long long __int128 *x420; /* { dg-error "" } */
int _Complex long signed __int128 *x421; /* { dg-error "" } */
int _Complex long unsigned __int128 *x422; /* { dg-error "" } */
int _Complex signed short __int128 *x423; /* { dg-error "" } */
int _Complex signed long __int128 *x424; /* { dg-error "" } */
int _Complex unsigned short __int128 *x425; /* { dg-error "" } */
int _Complex unsigned long __int128 *x426; /* { dg-error "" } */
long int long signed __int128 *x427; /* { dg-error "" } */
long int long unsigned __int128 *x428; /* { dg-error "" } */
long int long _Complex __int128 *x429; /* { dg-error "" } */
long int signed long __int128 *x430; /* { dg-error "" } */
long int signed _Complex __int128 *x431; /* { dg-error "" } */
long int unsigned long __int128 *x432; /* { dg-error "" } */
long int unsigned _Complex __int128 *x433; /* { dg-error "" } */
long int _Complex long __int128 *x434; /* { dg-error "" } */
long int _Complex signed __int128 *x435; /* { dg-error "" } */
long int _Complex unsigned __int128 *x436; /* { dg-error "" } */
long long int signed __int128 *x437; /* { dg-error "" } */
long long int unsigned __int128 *x438; /* { dg-error "" } */
long long int _Complex __int128 *x439; /* { dg-error "" } */
long long signed int __int128 *x440; /* { dg-error "" } */
long long signed _Complex __int128 *x441; /* { dg-error "" } */
long long unsigned int __int128 *x442; /* { dg-error "" } */
long long unsigned _Complex __int128 *x443; /* { dg-error "" } */
long long _Complex int __int128 *x444; /* { dg-error "" } */
long long _Complex signed __int128 *x445; /* { dg-error "" } */
long long _Complex unsigned __int128 *x446; /* { dg-error "" } */
long signed int long __int128 *x447; /* { dg-error "" } */
long signed int _Complex __int128 *x448; /* { dg-error "" } */
long signed long int __int128 *x449; /* { dg-error "" } */
long signed long _Complex __int128 *x450; /* { dg-error "" } */
long signed _Complex int __int128 *x451; /* { dg-error "" } */
long signed _Complex long __int128 *x452; /* { dg-error "" } */
long unsigned int long __int128 *x453; /* { dg-error "" } */
long unsigned int _Complex __int128 *x454; /* { dg-error "" } */
long unsigned long int __int128 *x455; /* { dg-error "" } */
long unsigned long _Complex __int128 *x456; /* { dg-error "" } */
long unsigned _Complex int __int128 *x457; /* { dg-error "" } */
long unsigned _Complex long __int128 *x458; /* { dg-error "" } */
long _Complex int long __int128 *x459; /* { dg-error "" } */
long _Complex int signed __int128 *x460; /* { dg-error "" } */
long _Complex int unsigned __int128 *x461; /* { dg-error "" } */
long _Complex long int __int128 *x462; /* { dg-error "" } */
long _Complex long signed __int128 *x463; /* { dg-error "" } */
long _Complex long unsigned __int128 *x464; /* { dg-error "" } */
long _Complex signed int __int128 *x465; /* { dg-error "" } */
long _Complex signed long __int128 *x466; /* { dg-error "" } */
long _Complex unsigned int __int128 *x467; /* { dg-error "" } */
long _Complex unsigned long __int128 *x468; /* { dg-error "" } */
signed short int _Complex __int128 *x469; /* { dg-error "" } */
signed short _Complex int __int128 *x470; /* { dg-error "" } */
signed int short _Complex __int128 *x471; /* { dg-error "" } */
signed int long long __int128 *x472; /* { dg-error "" } */
signed int long _Complex __int128 *x473; /* { dg-error "" } */
signed int _Complex short __int128 *x474; /* { dg-error "" } */
signed int _Complex long __int128 *x475; /* { dg-error "" } */
signed long int long __int128 *x476; /* { dg-error "" } */
signed long int _Complex __int128 *x477; /* { dg-error "" } */
signed long long int __int128 *x478; /* { dg-error "" } */
signed long long _Complex __int128 *x479; /* { dg-error "" } */
signed long _Complex int __int128 *x480; /* { dg-error "" } */
signed long _Complex long __int128 *x481; /* { dg-error "" } */
signed _Complex short int __int128 *x482; /* { dg-error "" } */
signed _Complex int short __int128 *x483; /* { dg-error "" } */
signed _Complex int long __int128 *x484; /* { dg-error "" } */
signed _Complex long int __int128 *x485; /* { dg-error "" } */
signed _Complex long long __int128 *x486; /* { dg-error "" } */
unsigned short int _Complex __int128 *x487; /* { dg-error "" } */
unsigned short _Complex int __int128 *x488; /* { dg-error "" } */
unsigned int short _Complex __int128 *x489; /* { dg-error "" } */
unsigned int long long __int128 *x490; /* { dg-error "" } */
unsigned int long _Complex __int128 *x491; /* { dg-error "" } */
unsigned int _Complex short __int128 *x492; /* { dg-error "" } */
unsigned int _Complex long __int128 *x493; /* { dg-error "" } */
unsigned long int long __int128 *x494; /* { dg-error "" } */
unsigned long int _Complex __int128 *x495; /* { dg-error "" } */
unsigned long long int __int128 *x496; /* { dg-error "" } */
unsigned long long _Complex __int128 *x497; /* { dg-error "" } */
unsigned long _Complex int __int128 *x498; /* { dg-error "" } */
unsigned long _Complex long __int128 *x499; /* { dg-error "" } */
unsigned _Complex short int __int128 *x500; /* { dg-error "" } */
unsigned _Complex int short __int128 *x501; /* { dg-error "" } */
unsigned _Complex int long __int128 *x502; /* { dg-error "" } */
unsigned _Complex long int __int128 *x503; /* { dg-error "" } */
unsigned _Complex long long __int128 *x504; /* { dg-error "" } */
_Complex short int signed __int128 *x505; /* { dg-error "" } */
_Complex short int unsigned __int128 *x506; /* { dg-error "" } */
_Complex short signed int __int128 *x507; /* { dg-error "" } */
_Complex short unsigned int __int128 *x508; /* { dg-error "" } */
_Complex int short signed __int128 *x509; /* { dg-error "" } */
_Complex int short unsigned __int128 *x510; /* { dg-error "" } */
_Complex int long long __int128 *x511; /* { dg-error "" } */
_Complex int long signed __int128 *x512; /* { dg-error "" } */
_Complex int long unsigned __int128 *x513; /* { dg-error "" } */
_Complex int signed short __int128 *x514; /* { dg-error "" } */
_Complex int signed long __int128 *x515; /* { dg-error "" } */
_Complex int unsigned short __int128 *x516; /* { dg-error "" } */
_Complex int unsigned long __int128 *x517; /* { dg-error "" } */
_Complex long int long __int128 *x518; /* { dg-error "" } */
_Complex long int signed __int128 *x519; /* { dg-error "" } */
_Complex long int unsigned __int128 *x520; /* { dg-error "" } */
_Complex long long int __int128 *x521; /* { dg-error "" } */
_Complex long long signed __int128 *x522; /* { dg-error "" } */
_Complex long long unsigned __int128 *x523; /* { dg-error "" } */
_Complex long signed int __int128 *x524; /* { dg-error "" } */
_Complex long signed long __int128 *x525; /* { dg-error "" } */
_Complex long unsigned int __int128 *x526; /* { dg-error "" } */
_Complex long unsigned long __int128 *x527; /* { dg-error "" } */
_Complex signed short int __int128 *x528; /* { dg-error "" } */
_Complex signed int short __int128 *x529; /* { dg-error "" } */
_Complex signed int long __int128 *x530; /* { dg-error "" } */
_Complex signed long int __int128 *x531; /* { dg-error "" } */
_Complex signed long long __int128 *x532; /* { dg-error "" } */
_Complex unsigned short int __int128 *x533; /* { dg-error "" } */
_Complex unsigned int short __int128 *x534; /* { dg-error "" } */
_Complex unsigned int long __int128 *x535; /* { dg-error "" } */
_Complex unsigned long int __int128 *x536; /* { dg-error "" } */
_Complex unsigned long long __int128 *x537; /* { dg-error "" } */
int long long signed _Complex __int128 *x538; /* { dg-error "" } */
int long long unsigned _Complex __int128 *x539; /* { dg-error "" } */
int long long _Complex signed __int128 *x540; /* { dg-error "" } */
int long long _Complex unsigned __int128 *x541; /* { dg-error "" } */
int long signed long _Complex __int128 *x542; /* { dg-error "" } */
int long signed _Complex long __int128 *x543; /* { dg-error "" } */
int long unsigned long _Complex __int128 *x544; /* { dg-error "" } */
int long unsigned _Complex long __int128 *x545; /* { dg-error "" } */
int long _Complex long signed __int128 *x546; /* { dg-error "" } */
int long _Complex long unsigned __int128 *x547; /* { dg-error "" } */
int long _Complex signed long __int128 *x548; /* { dg-error "" } */
int long _Complex unsigned long __int128 *x549; /* { dg-error "" } */
int signed long long _Complex __int128 *x550; /* { dg-error "" } */
int signed long _Complex long __int128 *x551; /* { dg-error "" } */
int signed _Complex long long __int128 *x552; /* { dg-error "" } */
int unsigned long long _Complex __int128 *x553; /* { dg-error "" } */
int unsigned long _Complex long __int128 *x554; /* { dg-error "" } */
int unsigned _Complex long long __int128 *x555; /* { dg-error "" } */
int _Complex long long signed __int128 *x556; /* { dg-error "" } */
int _Complex long long unsigned __int128 *x557; /* { dg-error "" } */
int _Complex long signed long __int128 *x558; /* { dg-error "" } */
int _Complex long unsigned long __int128 *x559; /* { dg-error "" } */
int _Complex signed long long __int128 *x560; /* { dg-error "" } */
int _Complex unsigned long long __int128 *x561; /* { dg-error "" } */
long int long signed _Complex __int128 *x562; /* { dg-error "" } */
long int long unsigned _Complex __int128 *x563; /* { dg-error "" } */
long int long _Complex signed __int128 *x564; /* { dg-error "" } */
long int long _Complex unsigned __int128 *x565; /* { dg-error "" } */
long int signed long _Complex __int128 *x566; /* { dg-error "" } */
long int signed _Complex long __int128 *x567; /* { dg-error "" } */
long int unsigned long _Complex __int128 *x568; /* { dg-error "" } */
long int unsigned _Complex long __int128 *x569; /* { dg-error "" } */
long int _Complex long signed __int128 *x570; /* { dg-error "" } */
long int _Complex long unsigned __int128 *x571; /* { dg-error "" } */
long int _Complex signed long __int128 *x572; /* { dg-error "" } */
long int _Complex unsigned long __int128 *x573; /* { dg-error "" } */
long long int signed _Complex __int128 *x574; /* { dg-error "" } */
long long int unsigned _Complex __int128 *x575; /* { dg-error "" } */
long long int _Complex signed __int128 *x576; /* { dg-error "" } */
long long int _Complex unsigned __int128 *x577; /* { dg-error "" } */
long long signed int _Complex __int128 *x578; /* { dg-error "" } */
long long signed _Complex int __int128 *x579; /* { dg-error "" } */
long long unsigned int _Complex __int128 *x580; /* { dg-error "" } */
long long unsigned _Complex int __int128 *x581; /* { dg-error "" } */
long long _Complex int signed __int128 *x582; /* { dg-error "" } */
long long _Complex int unsigned __int128 *x583; /* { dg-error "" } */
long long _Complex signed int __int128 *x584; /* { dg-error "" } */
long long _Complex unsigned int __int128 *x585; /* { dg-error "" } */
long signed int long _Complex __int128 *x586; /* { dg-error "" } */
long signed int _Complex long __int128 *x587; /* { dg-error "" } */
long signed long int _Complex __int128 *x588; /* { dg-error "" } */
long signed long _Complex int __int128 *x589; /* { dg-error "" } */
long signed _Complex int long __int128 *x590; /* { dg-error "" } */
long signed _Complex long int __int128 *x591; /* { dg-error "" } */
long unsigned int long _Complex __int128 *x592; /* { dg-error "" } */
long unsigned int _Complex long __int128 *x593; /* { dg-error "" } */
long unsigned long int _Complex __int128 *x594; /* { dg-error "" } */
long unsigned long _Complex int __int128 *x595; /* { dg-error "" } */
long unsigned _Complex int long __int128 *x596; /* { dg-error "" } */
long unsigned _Complex long int __int128 *x597; /* { dg-error "" } */
long _Complex int long signed __int128 *x598; /* { dg-error "" } */
long _Complex int long unsigned __int128 *x599; /* { dg-error "" } */
long _Complex int signed long __int128 *x600; /* { dg-error "" } */
long _Complex int unsigned long __int128 *x601; /* { dg-error "" } */
long _Complex long int signed __int128 *x602; /* { dg-error "" } */
long _Complex long int unsigned __int128 *x603; /* { dg-error "" } */
long _Complex long signed int __int128 *x604; /* { dg-error "" } */
long _Complex long unsigned int __int128 *x605; /* { dg-error "" } */
long _Complex signed int long __int128 *x606; /* { dg-error "" } */
long _Complex signed long int __int128 *x607; /* { dg-error "" } */
long _Complex unsigned int long __int128 *x608; /* { dg-error "" } */
long _Complex unsigned long int __int128 *x609; /* { dg-error "" } */
signed int long long _Complex __int128 *x610; /* { dg-error "" } */
signed int long _Complex long __int128 *x611; /* { dg-error "" } */
signed int _Complex long long __int128 *x612; /* { dg-error "" } */
signed long int long _Complex __int128 *x613; /* { dg-error "" } */
signed long int _Complex long __int128 *x614; /* { dg-error "" } */
signed long long int _Complex __int128 *x615; /* { dg-error "" } */
signed long long _Complex int __int128 *x616; /* { dg-error "" } */
signed long _Complex int long __int128 *x617; /* { dg-error "" } */
signed long _Complex long int __int128 *x618; /* { dg-error "" } */
signed _Complex int long long __int128 *x619; /* { dg-error "" } */
signed _Complex long int long __int128 *x620; /* { dg-error "" } */
signed _Complex long long int __int128 *x621; /* { dg-error "" } */
unsigned int long long _Complex __int128 *x622; /* { dg-error "" } */
unsigned int long _Complex long __int128 *x623; /* { dg-error "" } */
unsigned int _Complex long long __int128 *x624; /* { dg-error "" } */
unsigned long int long _Complex __int128 *x625; /* { dg-error "" } */
unsigned long int _Complex long __int128 *x626; /* { dg-error "" } */
unsigned long long int _Complex __int128 *x627; /* { dg-error "" } */
unsigned long long _Complex int __int128 *x628; /* { dg-error "" } */
unsigned long _Complex int long __int128 *x629; /* { dg-error "" } */
unsigned long _Complex long int __int128 *x630; /* { dg-error "" } */
unsigned _Complex int long long __int128 *x631; /* { dg-error "" } */
unsigned _Complex long int long __int128 *x632; /* { dg-error "" } */
unsigned _Complex long long int __int128 *x633; /* { dg-error "" } */
_Complex int long long signed __int128 *x634; /* { dg-error "" } */
_Complex int long long unsigned __int128 *x635; /* { dg-error "" } */
_Complex int long signed long __int128 *x636; /* { dg-error "" } */
_Complex int long unsigned long __int128 *x637; /* { dg-error "" } */
_Complex int signed long long __int128 *x638; /* { dg-error "" } */
_Complex int unsigned long long __int128 *x639; /* { dg-error "" } */
_Complex long int long signed __int128 *x640; /* { dg-error "" } */
_Complex long int long unsigned __int128 *x641; /* { dg-error "" } */
_Complex long int signed long __int128 *x642; /* { dg-error "" } */
_Complex long int unsigned long __int128 *x643; /* { dg-error "" } */
_Complex long long int signed __int128 *x644; /* { dg-error "" } */
_Complex long long int unsigned __int128 *x645; /* { dg-error "" } */
_Complex long long signed int __int128 *x646; /* { dg-error "" } */
_Complex long long unsigned int __int128 *x647; /* { dg-error "" } */
_Complex long signed int long __int128 *x648; /* { dg-error "" } */
_Complex long signed long int __int128 *x649; /* { dg-error "" } */
_Complex long unsigned int long __int128 *x650; /* { dg-error "" } */
_Complex long unsigned long int __int128 *x651; /* { dg-error "" } */
_Complex signed int long long __int128 *x652; /* { dg-error "" } */
_Complex signed long int long __int128 *x653; /* { dg-error "" } */
_Complex signed long long int __int128 *x654; /* { dg-error "" } */
_Complex unsigned int long long __int128 *x655; /* { dg-error "" } */
_Complex unsigned long int long __int128 *x656; /* { dg-error "" } */
_Complex unsigned long long int __int128 *x657; /* { dg-error "" } */
