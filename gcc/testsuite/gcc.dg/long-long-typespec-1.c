/* Test for valid and invalid combinations of type specifiers in C90
   with -Wno-long-long.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors -Wno-long-long" } */

typedef char type;
extern *x0;
void *x1;
char *x2;
short *x3;
int *x4;
long *x5;
float *x6;
double *x7;
signed *x8;
unsigned *x9;
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
char void *x20; /* { dg-error "" "char void" } */
char char *x21; /* { dg-error "" "char char" } */
char short *x22; /* { dg-error "" "char short" } */
char int *x23; /* { dg-error "" "char int" } */
char long *x24; /* { dg-error "" "char long" } */
char float *x25; /* { dg-error "" "char float" } */
char double *x26; /* { dg-error "" "char double" } */
char signed *x27;
char unsigned *x28;
short void *x29; /* { dg-error "" "short void" } */
short char *x30; /* { dg-error "" "short char" } */
short short *x31; /* { dg-error "" "short short" } */
short int *x32;
short long *x33; /* { dg-error "" "short long" } */
short float *x34; /* { dg-error "" "short float" } */
short double *x35; /* { dg-error "" "short double" } */
short signed *x36;
short unsigned *x37;
int void *x38; /* { dg-error "" "int void" } */
int char *x39; /* { dg-error "" "int char" } */
int short *x40;
int int *x41; /* { dg-error "" "int int" } */
int long *x42;
int float *x43; /* { dg-error "" "int float" } */
int double *x44; /* { dg-error "" "int double" } */
int signed *x45;
int unsigned *x46;
long void *x47; /* { dg-error "" "long void" } */
long char *x48; /* { dg-error "" "long char" } */
long short *x49; /* { dg-error "" "long short" } */
long int *x50;
long long *x51;
long float *x52; /* { dg-error "" "long float" } */
long double *x53;
long signed *x54;
long unsigned *x55;
float void *x56; /* { dg-error "" "float void" } */
float char *x57; /* { dg-error "" "float char" } */
float short *x58; /* { dg-error "" "float short" } */
float int *x59; /* { dg-error "" "float int" } */
float long *x60; /* { dg-error "" "float long" } */
float float *x61; /* { dg-error "" "float float" } */
float double *x62; /* { dg-error "" "float double" } */
float signed *x63; /* { dg-error "" "float signed" } */
float unsigned *x64; /* { dg-error "" "float unsigned" } */
double void *x65; /* { dg-error "" "double void" } */
double char *x66; /* { dg-error "" "double char" } */
double short *x67; /* { dg-error "" "double short" } */
double int *x68; /* { dg-error "" "double int" } */
double long *x69;
double float *x70; /* { dg-error "" "double float" } */
double double *x71; /* { dg-error "" "double double" } */
double signed *x72; /* { dg-error "" "double signed" } */
double unsigned *x73; /* { dg-error "" "double unsigned" } */
signed void *x74; /* { dg-error "" "signed void" } */
signed char *x75;
signed short *x76;
signed int *x77;
signed long *x78;
signed float *x79; /* { dg-error "" "signed float" } */
signed double *x80; /* { dg-error "" "signed double" } */
signed signed *x81; /* { dg-error "" "signed signed" } */
signed unsigned *x82; /* { dg-error "" "signed unsigned" } */
unsigned void *x83; /* { dg-error "" "unsigned void" } */
unsigned char *x84;
unsigned short *x85;
unsigned int *x86;
unsigned long *x87;
unsigned float *x88; /* { dg-error "" "unsigned float" } */
unsigned double *x89; /* { dg-error "" "unsigned double" } */
unsigned signed *x90; /* { dg-error "" "unsigned signed" } */
unsigned unsigned *x91; /* { dg-error "" "unsigned unsigned" } */
type void *x92; /* { dg-error "" "type void" } */
type char *x93; /* { dg-error "" "type char" } */
type short *x94; /* { dg-error "" "type short" } */
type int *x95; /* { dg-error "" "type int" } */
type long *x96; /* { dg-error "" "type long" } */
type float *x97; /* { dg-error "" "type float" } */
type double *x98; /* { dg-error "" "type double" } */
type signed *x99; /* { dg-error "" "type signed" } */
type unsigned *x100; /* { dg-error "" "type unsigned" } */
char signed void *x101; /* { dg-error "" "char signed void" } */
char signed char *x102; /* { dg-error "" "char signed char" } */
char signed short *x103; /* { dg-error "" "char signed short" } */
char signed int *x104; /* { dg-error "" "char signed int" } */
char signed long *x105; /* { dg-error "" "char signed long" } */
char signed float *x106; /* { dg-error "" "char signed float" } */
char signed double *x107; /* { dg-error "" "char signed double" } */
char signed signed *x108; /* { dg-error "" "char signed signed" } */
char signed unsigned *x109; /* { dg-error "" "char signed unsigned" } */
char unsigned void *x110; /* { dg-error "" "char unsigned void" } */
char unsigned char *x111; /* { dg-error "" "char unsigned char" } */
char unsigned short *x112; /* { dg-error "" "char unsigned short" } */
char unsigned int *x113; /* { dg-error "" "char unsigned int" } */
char unsigned long *x114; /* { dg-error "" "char unsigned long" } */
char unsigned float *x115; /* { dg-error "" "char unsigned float" } */
char unsigned double *x116; /* { dg-error "" "char unsigned double" } */
char unsigned signed *x117; /* { dg-error "" "char unsigned signed" } */
char unsigned unsigned *x118; /* { dg-error "" "char unsigned unsigned" } */
short int void *x119; /* { dg-error "" "short int void" } */
short int char *x120; /* { dg-error "" "short int char" } */
short int short *x121; /* { dg-error "" "short int short" } */
short int int *x122; /* { dg-error "" "short int int" } */
short int long *x123; /* { dg-error "" "short int long" } */
short int float *x124; /* { dg-error "" "short int float" } */
short int double *x125; /* { dg-error "" "short int double" } */
short int signed *x126;
short int unsigned *x127;
short signed void *x128; /* { dg-error "" "short signed void" } */
short signed char *x129; /* { dg-error "" "short signed char" } */
short signed short *x130; /* { dg-error "" "short signed short" } */
short signed int *x131;
short signed long *x132; /* { dg-error "" "short signed long" } */
short signed float *x133; /* { dg-error "" "short signed float" } */
short signed double *x134; /* { dg-error "" "short signed double" } */
short signed signed *x135; /* { dg-error "" "short signed signed" } */
short signed unsigned *x136; /* { dg-error "" "short signed unsigned" } */
short unsigned void *x137; /* { dg-error "" "short unsigned void" } */
short unsigned char *x138; /* { dg-error "" "short unsigned char" } */
short unsigned short *x139; /* { dg-error "" "short unsigned short" } */
short unsigned int *x140;
short unsigned long *x141; /* { dg-error "" "short unsigned long" } */
short unsigned float *x142; /* { dg-error "" "short unsigned float" } */
short unsigned double *x143; /* { dg-error "" "short unsigned double" } */
short unsigned signed *x144; /* { dg-error "" "short unsigned signed" } */
short unsigned unsigned *x145; /* { dg-error "" "short unsigned unsigned" } */
int short void *x146; /* { dg-error "" "int short void" } */
int short char *x147; /* { dg-error "" "int short char" } */
int short short *x148; /* { dg-error "" "int short short" } */
int short int *x149; /* { dg-error "" "int short int" } */
int short long *x150; /* { dg-error "" "int short long" } */
int short float *x151; /* { dg-error "" "int short float" } */
int short double *x152; /* { dg-error "" "int short double" } */
int short signed *x153;
int short unsigned *x154;
int long void *x155; /* { dg-error "" "int long void" } */
int long char *x156; /* { dg-error "" "int long char" } */
int long short *x157; /* { dg-error "" "int long short" } */
int long int *x158; /* { dg-error "" "int long int" } */
int long long *x159;
int long float *x160; /* { dg-error "" "int long float" } */
int long double *x161; /* { dg-error "" "int long double" } */
int long signed *x162;
int long unsigned *x163;
int signed void *x164; /* { dg-error "" "int signed void" } */
int signed char *x165; /* { dg-error "" "int signed char" } */
int signed short *x166;
int signed int *x167; /* { dg-error "" "int signed int" } */
int signed long *x168;
int signed float *x169; /* { dg-error "" "int signed float" } */
int signed double *x170; /* { dg-error "" "int signed double" } */
int signed signed *x171; /* { dg-error "" "int signed signed" } */
int signed unsigned *x172; /* { dg-error "" "int signed unsigned" } */
int unsigned void *x173; /* { dg-error "" "int unsigned void" } */
int unsigned char *x174; /* { dg-error "" "int unsigned char" } */
int unsigned short *x175;
int unsigned int *x176; /* { dg-error "" "int unsigned int" } */
int unsigned long *x177;
int unsigned float *x178; /* { dg-error "" "int unsigned float" } */
int unsigned double *x179; /* { dg-error "" "int unsigned double" } */
int unsigned signed *x180; /* { dg-error "" "int unsigned signed" } */
int unsigned unsigned *x181; /* { dg-error "" "int unsigned unsigned" } */
long int void *x182; /* { dg-error "" "long int void" } */
long int char *x183; /* { dg-error "" "long int char" } */
long int short *x184; /* { dg-error "" "long int short" } */
long int int *x185; /* { dg-error "" "long int int" } */
long int long *x186;
long int float *x187; /* { dg-error "" "long int float" } */
long int double *x188; /* { dg-error "" "long int double" } */
long int signed *x189;
long int unsigned *x190;
long long void *x191; /* { dg-error "" "long long void" } */
long long char *x192; /* { dg-error "" "long long char" } */
long long short *x193; /* { dg-error "" "long long short" } */
long long int *x194;
long long long *x195; /* { dg-error "" "long long long" } */
long long float *x196; /* { dg-error "" "long long float" } */
long long double *x197; /* { dg-error "" "long long double" } */
long long signed *x198;
long long unsigned *x199;
long double void *x200; /* { dg-error "" "long double void" } */
long double char *x201; /* { dg-error "" "long double char" } */
long double short *x202; /* { dg-error "" "long double short" } */
long double int *x203; /* { dg-error "" "long double int" } */
long double long *x204; /* { dg-error "" "long double long" } */
long double float *x205; /* { dg-error "" "long double float" } */
long double double *x206; /* { dg-error "" "long double double" } */
long double signed *x207; /* { dg-error "" "long double signed" } */
long double unsigned *x208; /* { dg-error "" "long double unsigned" } */
long signed void *x209; /* { dg-error "" "long signed void" } */
long signed char *x210; /* { dg-error "" "long signed char" } */
long signed short *x211; /* { dg-error "" "long signed short" } */
long signed int *x212;
long signed long *x213;
long signed float *x214; /* { dg-error "" "long signed float" } */
long signed double *x215; /* { dg-error "" "long signed double" } */
long signed signed *x216; /* { dg-error "" "long signed signed" } */
long signed unsigned *x217; /* { dg-error "" "long signed unsigned" } */
long unsigned void *x218; /* { dg-error "" "long unsigned void" } */
long unsigned char *x219; /* { dg-error "" "long unsigned char" } */
long unsigned short *x220; /* { dg-error "" "long unsigned short" } */
long unsigned int *x221;
long unsigned long *x222;
long unsigned float *x223; /* { dg-error "" "long unsigned float" } */
long unsigned double *x224; /* { dg-error "" "long unsigned double" } */
long unsigned signed *x225; /* { dg-error "" "long unsigned signed" } */
long unsigned unsigned *x226; /* { dg-error "" "long unsigned unsigned" } */
double long void *x227; /* { dg-error "" "double long void" } */
double long char *x228; /* { dg-error "" "double long char" } */
double long short *x229; /* { dg-error "" "double long short" } */
double long int *x230; /* { dg-error "" "double long int" } */
double long long *x231; /* { dg-error "" "double long long" } */
double long float *x232; /* { dg-error "" "double long float" } */
double long double *x233; /* { dg-error "" "double long double" } */
double long signed *x234; /* { dg-error "" "double long signed" } */
double long unsigned *x235; /* { dg-error "" "double long unsigned" } */
signed char void *x236; /* { dg-error "" "signed char void" } */
signed char char *x237; /* { dg-error "" "signed char char" } */
signed char short *x238; /* { dg-error "" "signed char short" } */
signed char int *x239; /* { dg-error "" "signed char int" } */
signed char long *x240; /* { dg-error "" "signed char long" } */
signed char float *x241; /* { dg-error "" "signed char float" } */
signed char double *x242; /* { dg-error "" "signed char double" } */
signed char signed *x243; /* { dg-error "" "signed char signed" } */
signed char unsigned *x244; /* { dg-error "" "signed char unsigned" } */
signed short void *x245; /* { dg-error "" "signed short void" } */
signed short char *x246; /* { dg-error "" "signed short char" } */
signed short short *x247; /* { dg-error "" "signed short short" } */
signed short int *x248;
signed short long *x249; /* { dg-error "" "signed short long" } */
signed short float *x250; /* { dg-error "" "signed short float" } */
signed short double *x251; /* { dg-error "" "signed short double" } */
signed short signed *x252; /* { dg-error "" "signed short signed" } */
signed short unsigned *x253; /* { dg-error "" "signed short unsigned" } */
signed int void *x254; /* { dg-error "" "signed int void" } */
signed int char *x255; /* { dg-error "" "signed int char" } */
signed int short *x256;
signed int int *x257; /* { dg-error "" "signed int int" } */
signed int long *x258;
signed int float *x259; /* { dg-error "" "signed int float" } */
signed int double *x260; /* { dg-error "" "signed int double" } */
signed int signed *x261; /* { dg-error "" "signed int signed" } */
signed int unsigned *x262; /* { dg-error "" "signed int unsigned" } */
signed long void *x263; /* { dg-error "" "signed long void" } */
signed long char *x264; /* { dg-error "" "signed long char" } */
signed long short *x265; /* { dg-error "" "signed long short" } */
signed long int *x266;
signed long long *x267;
signed long float *x268; /* { dg-error "" "signed long float" } */
signed long double *x269; /* { dg-error "" "signed long double" } */
signed long signed *x270; /* { dg-error "" "signed long signed" } */
signed long unsigned *x271; /* { dg-error "" "signed long unsigned" } */
unsigned char void *x272; /* { dg-error "" "unsigned char void" } */
unsigned char char *x273; /* { dg-error "" "unsigned char char" } */
unsigned char short *x274; /* { dg-error "" "unsigned char short" } */
unsigned char int *x275; /* { dg-error "" "unsigned char int" } */
unsigned char long *x276; /* { dg-error "" "unsigned char long" } */
unsigned char float *x277; /* { dg-error "" "unsigned char float" } */
unsigned char double *x278; /* { dg-error "" "unsigned char double" } */
unsigned char signed *x279; /* { dg-error "" "unsigned char signed" } */
unsigned char unsigned *x280; /* { dg-error "" "unsigned char unsigned" } */
unsigned short void *x281; /* { dg-error "" "unsigned short void" } */
unsigned short char *x282; /* { dg-error "" "unsigned short char" } */
unsigned short short *x283; /* { dg-error "" "unsigned short short" } */
unsigned short int *x284;
unsigned short long *x285; /* { dg-error "" "unsigned short long" } */
unsigned short float *x286; /* { dg-error "" "unsigned short float" } */
unsigned short double *x287; /* { dg-error "" "unsigned short double" } */
unsigned short signed *x288; /* { dg-error "" "unsigned short signed" } */
unsigned short unsigned *x289; /* { dg-error "" "unsigned short unsigned" } */
unsigned int void *x290; /* { dg-error "" "unsigned int void" } */
unsigned int char *x291; /* { dg-error "" "unsigned int char" } */
unsigned int short *x292;
unsigned int int *x293; /* { dg-error "" "unsigned int int" } */
unsigned int long *x294;
unsigned int float *x295; /* { dg-error "" "unsigned int float" } */
unsigned int double *x296; /* { dg-error "" "unsigned int double" } */
unsigned int signed *x297; /* { dg-error "" "unsigned int signed" } */
unsigned int unsigned *x298; /* { dg-error "" "unsigned int unsigned" } */
unsigned long void *x299; /* { dg-error "" "unsigned long void" } */
unsigned long char *x300; /* { dg-error "" "unsigned long char" } */
unsigned long short *x301; /* { dg-error "" "unsigned long short" } */
unsigned long int *x302;
unsigned long long *x303;
unsigned long float *x304; /* { dg-error "" "unsigned long float" } */
unsigned long double *x305; /* { dg-error "" "unsigned long double" } */
unsigned long signed *x306; /* { dg-error "" "unsigned long signed" } */
unsigned long unsigned *x307; /* { dg-error "" "unsigned long unsigned" } */
short int signed void *x308; /* { dg-error "" "short int signed void" } */
short int signed char *x309; /* { dg-error "" "short int signed char" } */
short int signed short *x310; /* { dg-error "" "short int signed short" } */
short int signed int *x311; /* { dg-error "" "short int signed int" } */
short int signed long *x312; /* { dg-error "" "short int signed long" } */
short int signed float *x313; /* { dg-error "" "short int signed float" } */
short int signed double *x314; /* { dg-error "" "short int signed double" } */
short int signed signed *x315; /* { dg-error "" "short int signed signed" } */
short int signed unsigned *x316; /* { dg-error "" "short int signed unsigned" } */
short int unsigned void *x317; /* { dg-error "" "short int unsigned void" } */
short int unsigned char *x318; /* { dg-error "" "short int unsigned char" } */
short int unsigned short *x319; /* { dg-error "" "short int unsigned short" } */
short int unsigned int *x320; /* { dg-error "" "short int unsigned int" } */
short int unsigned long *x321; /* { dg-error "" "short int unsigned long" } */
short int unsigned float *x322; /* { dg-error "" "short int unsigned float" } */
short int unsigned double *x323; /* { dg-error "" "short int unsigned double" } */
short int unsigned signed *x324; /* { dg-error "" "short int unsigned signed" } */
short int unsigned unsigned *x325; /* { dg-error "" "short int unsigned unsigned" } */
short signed int void *x326; /* { dg-error "" "short signed int void" } */
short signed int char *x327; /* { dg-error "" "short signed int char" } */
short signed int short *x328; /* { dg-error "" "short signed int short" } */
short signed int int *x329; /* { dg-error "" "short signed int int" } */
short signed int long *x330; /* { dg-error "" "short signed int long" } */
short signed int float *x331; /* { dg-error "" "short signed int float" } */
short signed int double *x332; /* { dg-error "" "short signed int double" } */
short signed int signed *x333; /* { dg-error "" "short signed int signed" } */
short signed int unsigned *x334; /* { dg-error "" "short signed int unsigned" } */
short unsigned int void *x335; /* { dg-error "" "short unsigned int void" } */
short unsigned int char *x336; /* { dg-error "" "short unsigned int char" } */
short unsigned int short *x337; /* { dg-error "" "short unsigned int short" } */
short unsigned int int *x338; /* { dg-error "" "short unsigned int int" } */
short unsigned int long *x339; /* { dg-error "" "short unsigned int long" } */
short unsigned int float *x340; /* { dg-error "" "short unsigned int float" } */
short unsigned int double *x341; /* { dg-error "" "short unsigned int double" } */
short unsigned int signed *x342; /* { dg-error "" "short unsigned int signed" } */
short unsigned int unsigned *x343; /* { dg-error "" "short unsigned int unsigned" } */
int short signed void *x344; /* { dg-error "" "int short signed void" } */
int short signed char *x345; /* { dg-error "" "int short signed char" } */
int short signed short *x346; /* { dg-error "" "int short signed short" } */
int short signed int *x347; /* { dg-error "" "int short signed int" } */
int short signed long *x348; /* { dg-error "" "int short signed long" } */
int short signed float *x349; /* { dg-error "" "int short signed float" } */
int short signed double *x350; /* { dg-error "" "int short signed double" } */
int short signed signed *x351; /* { dg-error "" "int short signed signed" } */
int short signed unsigned *x352; /* { dg-error "" "int short signed unsigned" } */
int short unsigned void *x353; /* { dg-error "" "int short unsigned void" } */
int short unsigned char *x354; /* { dg-error "" "int short unsigned char" } */
int short unsigned short *x355; /* { dg-error "" "int short unsigned short" } */
int short unsigned int *x356; /* { dg-error "" "int short unsigned int" } */
int short unsigned long *x357; /* { dg-error "" "int short unsigned long" } */
int short unsigned float *x358; /* { dg-error "" "int short unsigned float" } */
int short unsigned double *x359; /* { dg-error "" "int short unsigned double" } */
int short unsigned signed *x360; /* { dg-error "" "int short unsigned signed" } */
int short unsigned unsigned *x361; /* { dg-error "" "int short unsigned unsigned" } */
int long long void *x362; /* { dg-error "" "int long long void" } */
int long long char *x363; /* { dg-error "" "int long long char" } */
int long long short *x364; /* { dg-error "" "int long long short" } */
int long long int *x365; /* { dg-error "" "int long long int" } */
int long long long *x366; /* { dg-error "" "int long long long" } */
int long long float *x367; /* { dg-error "" "int long long float" } */
int long long double *x368; /* { dg-error "" "int long long double" } */
int long long signed *x369;
int long long unsigned *x370;
int long signed void *x371; /* { dg-error "" "int long signed void" } */
int long signed char *x372; /* { dg-error "" "int long signed char" } */
int long signed short *x373; /* { dg-error "" "int long signed short" } */
int long signed int *x374; /* { dg-error "" "int long signed int" } */
int long signed long *x375;
int long signed float *x376; /* { dg-error "" "int long signed float" } */
int long signed double *x377; /* { dg-error "" "int long signed double" } */
int long signed signed *x378; /* { dg-error "" "int long signed signed" } */
int long signed unsigned *x379; /* { dg-error "" "int long signed unsigned" } */
int long unsigned void *x380; /* { dg-error "" "int long unsigned void" } */
int long unsigned char *x381; /* { dg-error "" "int long unsigned char" } */
int long unsigned short *x382; /* { dg-error "" "int long unsigned short" } */
int long unsigned int *x383; /* { dg-error "" "int long unsigned int" } */
int long unsigned long *x384;
int long unsigned float *x385; /* { dg-error "" "int long unsigned float" } */
int long unsigned double *x386; /* { dg-error "" "int long unsigned double" } */
int long unsigned signed *x387; /* { dg-error "" "int long unsigned signed" } */
int long unsigned unsigned *x388; /* { dg-error "" "int long unsigned unsigned" } */
int signed short void *x389; /* { dg-error "" "int signed short void" } */
int signed short char *x390; /* { dg-error "" "int signed short char" } */
int signed short short *x391; /* { dg-error "" "int signed short short" } */
int signed short int *x392; /* { dg-error "" "int signed short int" } */
int signed short long *x393; /* { dg-error "" "int signed short long" } */
int signed short float *x394; /* { dg-error "" "int signed short float" } */
int signed short double *x395; /* { dg-error "" "int signed short double" } */
int signed short signed *x396; /* { dg-error "" "int signed short signed" } */
int signed short unsigned *x397; /* { dg-error "" "int signed short unsigned" } */
int signed long void *x398; /* { dg-error "" "int signed long void" } */
int signed long char *x399; /* { dg-error "" "int signed long char" } */
int signed long short *x400; /* { dg-error "" "int signed long short" } */
int signed long int *x401; /* { dg-error "" "int signed long int" } */
int signed long long *x402;
int signed long float *x403; /* { dg-error "" "int signed long float" } */
int signed long double *x404; /* { dg-error "" "int signed long double" } */
int signed long signed *x405; /* { dg-error "" "int signed long signed" } */
int signed long unsigned *x406; /* { dg-error "" "int signed long unsigned" } */
int unsigned short void *x407; /* { dg-error "" "int unsigned short void" } */
int unsigned short char *x408; /* { dg-error "" "int unsigned short char" } */
int unsigned short short *x409; /* { dg-error "" "int unsigned short short" } */
int unsigned short int *x410; /* { dg-error "" "int unsigned short int" } */
int unsigned short long *x411; /* { dg-error "" "int unsigned short long" } */
int unsigned short float *x412; /* { dg-error "" "int unsigned short float" } */
int unsigned short double *x413; /* { dg-error "" "int unsigned short double" } */
int unsigned short signed *x414; /* { dg-error "" "int unsigned short signed" } */
int unsigned short unsigned *x415; /* { dg-error "" "int unsigned short unsigned" } */
int unsigned long void *x416; /* { dg-error "" "int unsigned long void" } */
int unsigned long char *x417; /* { dg-error "" "int unsigned long char" } */
int unsigned long short *x418; /* { dg-error "" "int unsigned long short" } */
int unsigned long int *x419; /* { dg-error "" "int unsigned long int" } */
int unsigned long long *x420;
int unsigned long float *x421; /* { dg-error "" "int unsigned long float" } */
int unsigned long double *x422; /* { dg-error "" "int unsigned long double" } */
int unsigned long signed *x423; /* { dg-error "" "int unsigned long signed" } */
int unsigned long unsigned *x424; /* { dg-error "" "int unsigned long unsigned" } */
long int long void *x425; /* { dg-error "" "long int long void" } */
long int long char *x426; /* { dg-error "" "long int long char" } */
long int long short *x427; /* { dg-error "" "long int long short" } */
long int long int *x428; /* { dg-error "" "long int long int" } */
long int long long *x429; /* { dg-error "" "long int long long" } */
long int long float *x430; /* { dg-error "" "long int long float" } */
long int long double *x431; /* { dg-error "" "long int long double" } */
long int long signed *x432;
long int long unsigned *x433;
long int signed void *x434; /* { dg-error "" "long int signed void" } */
long int signed char *x435; /* { dg-error "" "long int signed char" } */
long int signed short *x436; /* { dg-error "" "long int signed short" } */
long int signed int *x437; /* { dg-error "" "long int signed int" } */
long int signed long *x438;
long int signed float *x439; /* { dg-error "" "long int signed float" } */
long int signed double *x440; /* { dg-error "" "long int signed double" } */
long int signed signed *x441; /* { dg-error "" "long int signed signed" } */
long int signed unsigned *x442; /* { dg-error "" "long int signed unsigned" } */
long int unsigned void *x443; /* { dg-error "" "long int unsigned void" } */
long int unsigned char *x444; /* { dg-error "" "long int unsigned char" } */
long int unsigned short *x445; /* { dg-error "" "long int unsigned short" } */
long int unsigned int *x446; /* { dg-error "" "long int unsigned int" } */
long int unsigned long *x447;
long int unsigned float *x448; /* { dg-error "" "long int unsigned float" } */
long int unsigned double *x449; /* { dg-error "" "long int unsigned double" } */
long int unsigned signed *x450; /* { dg-error "" "long int unsigned signed" } */
long int unsigned unsigned *x451; /* { dg-error "" "long int unsigned unsigned" } */
long long int void *x452; /* { dg-error "" "long long int void" } */
long long int char *x453; /* { dg-error "" "long long int char" } */
long long int short *x454; /* { dg-error "" "long long int short" } */
long long int int *x455; /* { dg-error "" "long long int int" } */
long long int long *x456; /* { dg-error "" "long long int long" } */
long long int float *x457; /* { dg-error "" "long long int float" } */
long long int double *x458; /* { dg-error "" "long long int double" } */
long long int signed *x459;
long long int unsigned *x460;
long long signed void *x461; /* { dg-error "" "long long signed void" } */
long long signed char *x462; /* { dg-error "" "long long signed char" } */
long long signed short *x463; /* { dg-error "" "long long signed short" } */
long long signed int *x464;
long long signed long *x465; /* { dg-error "" "long long signed long" } */
long long signed float *x466; /* { dg-error "" "long long signed float" } */
long long signed double *x467; /* { dg-error "" "long long signed double" } */
long long signed signed *x468; /* { dg-error "" "long long signed signed" } */
long long signed unsigned *x469; /* { dg-error "" "long long signed unsigned" } */
long long unsigned void *x470; /* { dg-error "" "long long unsigned void" } */
long long unsigned char *x471; /* { dg-error "" "long long unsigned char" } */
long long unsigned short *x472; /* { dg-error "" "long long unsigned short" } */
long long unsigned int *x473;
long long unsigned long *x474; /* { dg-error "" "long long unsigned long" } */
long long unsigned float *x475; /* { dg-error "" "long long unsigned float" } */
long long unsigned double *x476; /* { dg-error "" "long long unsigned double" } */
long long unsigned signed *x477; /* { dg-error "" "long long unsigned signed" } */
long long unsigned unsigned *x478; /* { dg-error "" "long long unsigned unsigned" } */
long signed int void *x479; /* { dg-error "" "long signed int void" } */
long signed int char *x480; /* { dg-error "" "long signed int char" } */
long signed int short *x481; /* { dg-error "" "long signed int short" } */
long signed int int *x482; /* { dg-error "" "long signed int int" } */
long signed int long *x483;
long signed int float *x484; /* { dg-error "" "long signed int float" } */
long signed int double *x485; /* { dg-error "" "long signed int double" } */
long signed int signed *x486; /* { dg-error "" "long signed int signed" } */
long signed int unsigned *x487; /* { dg-error "" "long signed int unsigned" } */
long signed long void *x488; /* { dg-error "" "long signed long void" } */
long signed long char *x489; /* { dg-error "" "long signed long char" } */
long signed long short *x490; /* { dg-error "" "long signed long short" } */
long signed long int *x491;
long signed long long *x492; /* { dg-error "" "long signed long long" } */
long signed long float *x493; /* { dg-error "" "long signed long float" } */
long signed long double *x494; /* { dg-error "" "long signed long double" } */
long signed long signed *x495; /* { dg-error "" "long signed long signed" } */
long signed long unsigned *x496; /* { dg-error "" "long signed long unsigned" } */
long unsigned int void *x497; /* { dg-error "" "long unsigned int void" } */
long unsigned int char *x498; /* { dg-error "" "long unsigned int char" } */
long unsigned int short *x499; /* { dg-error "" "long unsigned int short" } */
long unsigned int int *x500; /* { dg-error "" "long unsigned int int" } */
long unsigned int long *x501;
long unsigned int float *x502; /* { dg-error "" "long unsigned int float" } */
long unsigned int double *x503; /* { dg-error "" "long unsigned int double" } */
long unsigned int signed *x504; /* { dg-error "" "long unsigned int signed" } */
long unsigned int unsigned *x505; /* { dg-error "" "long unsigned int unsigned" } */
long unsigned long void *x506; /* { dg-error "" "long unsigned long void" } */
long unsigned long char *x507; /* { dg-error "" "long unsigned long char" } */
long unsigned long short *x508; /* { dg-error "" "long unsigned long short" } */
long unsigned long int *x509;
long unsigned long long *x510; /* { dg-error "" "long unsigned long long" } */
long unsigned long float *x511; /* { dg-error "" "long unsigned long float" } */
long unsigned long double *x512; /* { dg-error "" "long unsigned long double" } */
long unsigned long signed *x513; /* { dg-error "" "long unsigned long signed" } */
long unsigned long unsigned *x514; /* { dg-error "" "long unsigned long unsigned" } */
signed short int void *x515; /* { dg-error "" "signed short int void" } */
signed short int char *x516; /* { dg-error "" "signed short int char" } */
signed short int short *x517; /* { dg-error "" "signed short int short" } */
signed short int int *x518; /* { dg-error "" "signed short int int" } */
signed short int long *x519; /* { dg-error "" "signed short int long" } */
signed short int float *x520; /* { dg-error "" "signed short int float" } */
signed short int double *x521; /* { dg-error "" "signed short int double" } */
signed short int signed *x522; /* { dg-error "" "signed short int signed" } */
signed short int unsigned *x523; /* { dg-error "" "signed short int unsigned" } */
signed int short void *x524; /* { dg-error "" "signed int short void" } */
signed int short char *x525; /* { dg-error "" "signed int short char" } */
signed int short short *x526; /* { dg-error "" "signed int short short" } */
signed int short int *x527; /* { dg-error "" "signed int short int" } */
signed int short long *x528; /* { dg-error "" "signed int short long" } */
signed int short float *x529; /* { dg-error "" "signed int short float" } */
signed int short double *x530; /* { dg-error "" "signed int short double" } */
signed int short signed *x531; /* { dg-error "" "signed int short signed" } */
signed int short unsigned *x532; /* { dg-error "" "signed int short unsigned" } */
signed int long void *x533; /* { dg-error "" "signed int long void" } */
signed int long char *x534; /* { dg-error "" "signed int long char" } */
signed int long short *x535; /* { dg-error "" "signed int long short" } */
signed int long int *x536; /* { dg-error "" "signed int long int" } */
signed int long long *x537;
signed int long float *x538; /* { dg-error "" "signed int long float" } */
signed int long double *x539; /* { dg-error "" "signed int long double" } */
signed int long signed *x540; /* { dg-error "" "signed int long signed" } */
signed int long unsigned *x541; /* { dg-error "" "signed int long unsigned" } */
signed long int void *x542; /* { dg-error "" "signed long int void" } */
signed long int char *x543; /* { dg-error "" "signed long int char" } */
signed long int short *x544; /* { dg-error "" "signed long int short" } */
signed long int int *x545; /* { dg-error "" "signed long int int" } */
signed long int long *x546;
signed long int float *x547; /* { dg-error "" "signed long int float" } */
signed long int double *x548; /* { dg-error "" "signed long int double" } */
signed long int signed *x549; /* { dg-error "" "signed long int signed" } */
signed long int unsigned *x550; /* { dg-error "" "signed long int unsigned" } */
signed long long void *x551; /* { dg-error "" "signed long long void" } */
signed long long char *x552; /* { dg-error "" "signed long long char" } */
signed long long short *x553; /* { dg-error "" "signed long long short" } */
signed long long int *x554;
signed long long long *x555; /* { dg-error "" "signed long long long" } */
signed long long float *x556; /* { dg-error "" "signed long long float" } */
signed long long double *x557; /* { dg-error "" "signed long long double" } */
signed long long signed *x558; /* { dg-error "" "signed long long signed" } */
signed long long unsigned *x559; /* { dg-error "" "signed long long unsigned" } */
unsigned short int void *x560; /* { dg-error "" "unsigned short int void" } */
unsigned short int char *x561; /* { dg-error "" "unsigned short int char" } */
unsigned short int short *x562; /* { dg-error "" "unsigned short int short" } */
unsigned short int int *x563; /* { dg-error "" "unsigned short int int" } */
unsigned short int long *x564; /* { dg-error "" "unsigned short int long" } */
unsigned short int float *x565; /* { dg-error "" "unsigned short int float" } */
unsigned short int double *x566; /* { dg-error "" "unsigned short int double" } */
unsigned short int signed *x567; /* { dg-error "" "unsigned short int signed" } */
unsigned short int unsigned *x568; /* { dg-error "" "unsigned short int unsigned" } */
unsigned int short void *x569; /* { dg-error "" "unsigned int short void" } */
unsigned int short char *x570; /* { dg-error "" "unsigned int short char" } */
unsigned int short short *x571; /* { dg-error "" "unsigned int short short" } */
unsigned int short int *x572; /* { dg-error "" "unsigned int short int" } */
unsigned int short long *x573; /* { dg-error "" "unsigned int short long" } */
unsigned int short float *x574; /* { dg-error "" "unsigned int short float" } */
unsigned int short double *x575; /* { dg-error "" "unsigned int short double" } */
unsigned int short signed *x576; /* { dg-error "" "unsigned int short signed" } */
unsigned int short unsigned *x577; /* { dg-error "" "unsigned int short unsigned" } */
unsigned int long void *x578; /* { dg-error "" "unsigned int long void" } */
unsigned int long char *x579; /* { dg-error "" "unsigned int long char" } */
unsigned int long short *x580; /* { dg-error "" "unsigned int long short" } */
unsigned int long int *x581; /* { dg-error "" "unsigned int long int" } */
unsigned int long long *x582;
unsigned int long float *x583; /* { dg-error "" "unsigned int long float" } */
unsigned int long double *x584; /* { dg-error "" "unsigned int long double" } */
unsigned int long signed *x585; /* { dg-error "" "unsigned int long signed" } */
unsigned int long unsigned *x586; /* { dg-error "" "unsigned int long unsigned" } */
unsigned long int void *x587; /* { dg-error "" "unsigned long int void" } */
unsigned long int char *x588; /* { dg-error "" "unsigned long int char" } */
unsigned long int short *x589; /* { dg-error "" "unsigned long int short" } */
unsigned long int int *x590; /* { dg-error "" "unsigned long int int" } */
unsigned long int long *x591;
unsigned long int float *x592; /* { dg-error "" "unsigned long int float" } */
unsigned long int double *x593; /* { dg-error "" "unsigned long int double" } */
unsigned long int signed *x594; /* { dg-error "" "unsigned long int signed" } */
unsigned long int unsigned *x595; /* { dg-error "" "unsigned long int unsigned" } */
unsigned long long void *x596; /* { dg-error "" "unsigned long long void" } */
unsigned long long char *x597; /* { dg-error "" "unsigned long long char" } */
unsigned long long short *x598; /* { dg-error "" "unsigned long long short" } */
unsigned long long int *x599;
unsigned long long long *x600; /* { dg-error "" "unsigned long long long" } */
unsigned long long float *x601; /* { dg-error "" "unsigned long long float" } */
unsigned long long double *x602; /* { dg-error "" "unsigned long long double" } */
unsigned long long signed *x603; /* { dg-error "" "unsigned long long signed" } */
unsigned long long unsigned *x604; /* { dg-error "" "unsigned long long unsigned" } */
int long long signed void *x605; /* { dg-error "" "int long long signed void" } */
int long long signed char *x606; /* { dg-error "" "int long long signed char" } */
int long long signed short *x607; /* { dg-error "" "int long long signed short" } */
int long long signed int *x608; /* { dg-error "" "int long long signed int" } */
int long long signed long *x609; /* { dg-error "" "int long long signed long" } */
int long long signed float *x610; /* { dg-error "" "int long long signed float" } */
int long long signed double *x611; /* { dg-error "" "int long long signed double" } */
int long long signed signed *x612; /* { dg-error "" "int long long signed signed" } */
int long long signed unsigned *x613; /* { dg-error "" "int long long signed unsigned" } */
int long long unsigned void *x614; /* { dg-error "" "int long long unsigned void" } */
int long long unsigned char *x615; /* { dg-error "" "int long long unsigned char" } */
int long long unsigned short *x616; /* { dg-error "" "int long long unsigned short" } */
int long long unsigned int *x617; /* { dg-error "" "int long long unsigned int" } */
int long long unsigned long *x618; /* { dg-error "" "int long long unsigned long" } */
int long long unsigned float *x619; /* { dg-error "" "int long long unsigned float" } */
int long long unsigned double *x620; /* { dg-error "" "int long long unsigned double" } */
int long long unsigned signed *x621; /* { dg-error "" "int long long unsigned signed" } */
int long long unsigned unsigned *x622; /* { dg-error "" "int long long unsigned unsigned" } */
int long signed long void *x623; /* { dg-error "" "int long signed long void" } */
int long signed long char *x624; /* { dg-error "" "int long signed long char" } */
int long signed long short *x625; /* { dg-error "" "int long signed long short" } */
int long signed long int *x626; /* { dg-error "" "int long signed long int" } */
int long signed long long *x627; /* { dg-error "" "int long signed long long" } */
int long signed long float *x628; /* { dg-error "" "int long signed long float" } */
int long signed long double *x629; /* { dg-error "" "int long signed long double" } */
int long signed long signed *x630; /* { dg-error "" "int long signed long signed" } */
int long signed long unsigned *x631; /* { dg-error "" "int long signed long unsigned" } */
int long unsigned long void *x632; /* { dg-error "" "int long unsigned long void" } */
int long unsigned long char *x633; /* { dg-error "" "int long unsigned long char" } */
int long unsigned long short *x634; /* { dg-error "" "int long unsigned long short" } */
int long unsigned long int *x635; /* { dg-error "" "int long unsigned long int" } */
int long unsigned long long *x636; /* { dg-error "" "int long unsigned long long" } */
int long unsigned long float *x637; /* { dg-error "" "int long unsigned long float" } */
int long unsigned long double *x638; /* { dg-error "" "int long unsigned long double" } */
int long unsigned long signed *x639; /* { dg-error "" "int long unsigned long signed" } */
int long unsigned long unsigned *x640; /* { dg-error "" "int long unsigned long unsigned" } */
int signed long long void *x641; /* { dg-error "" "int signed long long void" } */
int signed long long char *x642; /* { dg-error "" "int signed long long char" } */
int signed long long short *x643; /* { dg-error "" "int signed long long short" } */
int signed long long int *x644; /* { dg-error "" "int signed long long int" } */
int signed long long long *x645; /* { dg-error "" "int signed long long long" } */
int signed long long float *x646; /* { dg-error "" "int signed long long float" } */
int signed long long double *x647; /* { dg-error "" "int signed long long double" } */
int signed long long signed *x648; /* { dg-error "" "int signed long long signed" } */
int signed long long unsigned *x649; /* { dg-error "" "int signed long long unsigned" } */
int unsigned long long void *x650; /* { dg-error "" "int unsigned long long void" } */
int unsigned long long char *x651; /* { dg-error "" "int unsigned long long char" } */
int unsigned long long short *x652; /* { dg-error "" "int unsigned long long short" } */
int unsigned long long int *x653; /* { dg-error "" "int unsigned long long int" } */
int unsigned long long long *x654; /* { dg-error "" "int unsigned long long long" } */
int unsigned long long float *x655; /* { dg-error "" "int unsigned long long float" } */
int unsigned long long double *x656; /* { dg-error "" "int unsigned long long double" } */
int unsigned long long signed *x657; /* { dg-error "" "int unsigned long long signed" } */
int unsigned long long unsigned *x658; /* { dg-error "" "int unsigned long long unsigned" } */
long int long signed void *x659; /* { dg-error "" "long int long signed void" } */
long int long signed char *x660; /* { dg-error "" "long int long signed char" } */
long int long signed short *x661; /* { dg-error "" "long int long signed short" } */
long int long signed int *x662; /* { dg-error "" "long int long signed int" } */
long int long signed long *x663; /* { dg-error "" "long int long signed long" } */
long int long signed float *x664; /* { dg-error "" "long int long signed float" } */
long int long signed double *x665; /* { dg-error "" "long int long signed double" } */
long int long signed signed *x666; /* { dg-error "" "long int long signed signed" } */
long int long signed unsigned *x667; /* { dg-error "" "long int long signed unsigned" } */
long int long unsigned void *x668; /* { dg-error "" "long int long unsigned void" } */
long int long unsigned char *x669; /* { dg-error "" "long int long unsigned char" } */
long int long unsigned short *x670; /* { dg-error "" "long int long unsigned short" } */
long int long unsigned int *x671; /* { dg-error "" "long int long unsigned int" } */
long int long unsigned long *x672; /* { dg-error "" "long int long unsigned long" } */
long int long unsigned float *x673; /* { dg-error "" "long int long unsigned float" } */
long int long unsigned double *x674; /* { dg-error "" "long int long unsigned double" } */
long int long unsigned signed *x675; /* { dg-error "" "long int long unsigned signed" } */
long int long unsigned unsigned *x676; /* { dg-error "" "long int long unsigned unsigned" } */
long int signed long void *x677; /* { dg-error "" "long int signed long void" } */
long int signed long char *x678; /* { dg-error "" "long int signed long char" } */
long int signed long short *x679; /* { dg-error "" "long int signed long short" } */
long int signed long int *x680; /* { dg-error "" "long int signed long int" } */
long int signed long long *x681; /* { dg-error "" "long int signed long long" } */
long int signed long float *x682; /* { dg-error "" "long int signed long float" } */
long int signed long double *x683; /* { dg-error "" "long int signed long double" } */
long int signed long signed *x684; /* { dg-error "" "long int signed long signed" } */
long int signed long unsigned *x685; /* { dg-error "" "long int signed long unsigned" } */
long int unsigned long void *x686; /* { dg-error "" "long int unsigned long void" } */
long int unsigned long char *x687; /* { dg-error "" "long int unsigned long char" } */
long int unsigned long short *x688; /* { dg-error "" "long int unsigned long short" } */
long int unsigned long int *x689; /* { dg-error "" "long int unsigned long int" } */
long int unsigned long long *x690; /* { dg-error "" "long int unsigned long long" } */
long int unsigned long float *x691; /* { dg-error "" "long int unsigned long float" } */
long int unsigned long double *x692; /* { dg-error "" "long int unsigned long double" } */
long int unsigned long signed *x693; /* { dg-error "" "long int unsigned long signed" } */
long int unsigned long unsigned *x694; /* { dg-error "" "long int unsigned long unsigned" } */
long long int signed void *x695; /* { dg-error "" "long long int signed void" } */
long long int signed char *x696; /* { dg-error "" "long long int signed char" } */
long long int signed short *x697; /* { dg-error "" "long long int signed short" } */
long long int signed int *x698; /* { dg-error "" "long long int signed int" } */
long long int signed long *x699; /* { dg-error "" "long long int signed long" } */
long long int signed float *x700; /* { dg-error "" "long long int signed float" } */
long long int signed double *x701; /* { dg-error "" "long long int signed double" } */
long long int signed signed *x702; /* { dg-error "" "long long int signed signed" } */
long long int signed unsigned *x703; /* { dg-error "" "long long int signed unsigned" } */
long long int unsigned void *x704; /* { dg-error "" "long long int unsigned void" } */
long long int unsigned char *x705; /* { dg-error "" "long long int unsigned char" } */
long long int unsigned short *x706; /* { dg-error "" "long long int unsigned short" } */
long long int unsigned int *x707; /* { dg-error "" "long long int unsigned int" } */
long long int unsigned long *x708; /* { dg-error "" "long long int unsigned long" } */
long long int unsigned float *x709; /* { dg-error "" "long long int unsigned float" } */
long long int unsigned double *x710; /* { dg-error "" "long long int unsigned double" } */
long long int unsigned signed *x711; /* { dg-error "" "long long int unsigned signed" } */
long long int unsigned unsigned *x712; /* { dg-error "" "long long int unsigned unsigned" } */
long long signed int void *x713; /* { dg-error "" "long long signed int void" } */
long long signed int char *x714; /* { dg-error "" "long long signed int char" } */
long long signed int short *x715; /* { dg-error "" "long long signed int short" } */
long long signed int int *x716; /* { dg-error "" "long long signed int int" } */
long long signed int long *x717; /* { dg-error "" "long long signed int long" } */
long long signed int float *x718; /* { dg-error "" "long long signed int float" } */
long long signed int double *x719; /* { dg-error "" "long long signed int double" } */
long long signed int signed *x720; /* { dg-error "" "long long signed int signed" } */
long long signed int unsigned *x721; /* { dg-error "" "long long signed int unsigned" } */
long long unsigned int void *x722; /* { dg-error "" "long long unsigned int void" } */
long long unsigned int char *x723; /* { dg-error "" "long long unsigned int char" } */
long long unsigned int short *x724; /* { dg-error "" "long long unsigned int short" } */
long long unsigned int int *x725; /* { dg-error "" "long long unsigned int int" } */
long long unsigned int long *x726; /* { dg-error "" "long long unsigned int long" } */
long long unsigned int float *x727; /* { dg-error "" "long long unsigned int float" } */
long long unsigned int double *x728; /* { dg-error "" "long long unsigned int double" } */
long long unsigned int signed *x729; /* { dg-error "" "long long unsigned int signed" } */
long long unsigned int unsigned *x730; /* { dg-error "" "long long unsigned int unsigned" } */
long signed int long void *x731; /* { dg-error "" "long signed int long void" } */
long signed int long char *x732; /* { dg-error "" "long signed int long char" } */
long signed int long short *x733; /* { dg-error "" "long signed int long short" } */
long signed int long int *x734; /* { dg-error "" "long signed int long int" } */
long signed int long long *x735; /* { dg-error "" "long signed int long long" } */
long signed int long float *x736; /* { dg-error "" "long signed int long float" } */
long signed int long double *x737; /* { dg-error "" "long signed int long double" } */
long signed int long signed *x738; /* { dg-error "" "long signed int long signed" } */
long signed int long unsigned *x739; /* { dg-error "" "long signed int long unsigned" } */
long signed long int void *x740; /* { dg-error "" "long signed long int void" } */
long signed long int char *x741; /* { dg-error "" "long signed long int char" } */
long signed long int short *x742; /* { dg-error "" "long signed long int short" } */
long signed long int int *x743; /* { dg-error "" "long signed long int int" } */
long signed long int long *x744; /* { dg-error "" "long signed long int long" } */
long signed long int float *x745; /* { dg-error "" "long signed long int float" } */
long signed long int double *x746; /* { dg-error "" "long signed long int double" } */
long signed long int signed *x747; /* { dg-error "" "long signed long int signed" } */
long signed long int unsigned *x748; /* { dg-error "" "long signed long int unsigned" } */
long unsigned int long void *x749; /* { dg-error "" "long unsigned int long void" } */
long unsigned int long char *x750; /* { dg-error "" "long unsigned int long char" } */
long unsigned int long short *x751; /* { dg-error "" "long unsigned int long short" } */
long unsigned int long int *x752; /* { dg-error "" "long unsigned int long int" } */
long unsigned int long long *x753; /* { dg-error "" "long unsigned int long long" } */
long unsigned int long float *x754; /* { dg-error "" "long unsigned int long float" } */
long unsigned int long double *x755; /* { dg-error "" "long unsigned int long double" } */
long unsigned int long signed *x756; /* { dg-error "" "long unsigned int long signed" } */
long unsigned int long unsigned *x757; /* { dg-error "" "long unsigned int long unsigned" } */
long unsigned long int void *x758; /* { dg-error "" "long unsigned long int void" } */
long unsigned long int char *x759; /* { dg-error "" "long unsigned long int char" } */
long unsigned long int short *x760; /* { dg-error "" "long unsigned long int short" } */
long unsigned long int int *x761; /* { dg-error "" "long unsigned long int int" } */
long unsigned long int long *x762; /* { dg-error "" "long unsigned long int long" } */
long unsigned long int float *x763; /* { dg-error "" "long unsigned long int float" } */
long unsigned long int double *x764; /* { dg-error "" "long unsigned long int double" } */
long unsigned long int signed *x765; /* { dg-error "" "long unsigned long int signed" } */
long unsigned long int unsigned *x766; /* { dg-error "" "long unsigned long int unsigned" } */
signed int long long void *x767; /* { dg-error "" "signed int long long void" } */
signed int long long char *x768; /* { dg-error "" "signed int long long char" } */
signed int long long short *x769; /* { dg-error "" "signed int long long short" } */
signed int long long int *x770; /* { dg-error "" "signed int long long int" } */
signed int long long long *x771; /* { dg-error "" "signed int long long long" } */
signed int long long float *x772; /* { dg-error "" "signed int long long float" } */
signed int long long double *x773; /* { dg-error "" "signed int long long double" } */
signed int long long signed *x774; /* { dg-error "" "signed int long long signed" } */
signed int long long unsigned *x775; /* { dg-error "" "signed int long long unsigned" } */
signed long int long void *x776; /* { dg-error "" "signed long int long void" } */
signed long int long char *x777; /* { dg-error "" "signed long int long char" } */
signed long int long short *x778; /* { dg-error "" "signed long int long short" } */
signed long int long int *x779; /* { dg-error "" "signed long int long int" } */
signed long int long long *x780; /* { dg-error "" "signed long int long long" } */
signed long int long float *x781; /* { dg-error "" "signed long int long float" } */
signed long int long double *x782; /* { dg-error "" "signed long int long double" } */
signed long int long signed *x783; /* { dg-error "" "signed long int long signed" } */
signed long int long unsigned *x784; /* { dg-error "" "signed long int long unsigned" } */
signed long long int void *x785; /* { dg-error "" "signed long long int void" } */
signed long long int char *x786; /* { dg-error "" "signed long long int char" } */
signed long long int short *x787; /* { dg-error "" "signed long long int short" } */
signed long long int int *x788; /* { dg-error "" "signed long long int int" } */
signed long long int long *x789; /* { dg-error "" "signed long long int long" } */
signed long long int float *x790; /* { dg-error "" "signed long long int float" } */
signed long long int double *x791; /* { dg-error "" "signed long long int double" } */
signed long long int signed *x792; /* { dg-error "" "signed long long int signed" } */
signed long long int unsigned *x793; /* { dg-error "" "signed long long int unsigned" } */
unsigned int long long void *x794; /* { dg-error "" "unsigned int long long void" } */
unsigned int long long char *x795; /* { dg-error "" "unsigned int long long char" } */
unsigned int long long short *x796; /* { dg-error "" "unsigned int long long short" } */
unsigned int long long int *x797; /* { dg-error "" "unsigned int long long int" } */
unsigned int long long long *x798; /* { dg-error "" "unsigned int long long long" } */
unsigned int long long float *x799; /* { dg-error "" "unsigned int long long float" } */
unsigned int long long double *x800; /* { dg-error "" "unsigned int long long double" } */
unsigned int long long signed *x801; /* { dg-error "" "unsigned int long long signed" } */
unsigned int long long unsigned *x802; /* { dg-error "" "unsigned int long long unsigned" } */
unsigned long int long void *x803; /* { dg-error "" "unsigned long int long void" } */
unsigned long int long char *x804; /* { dg-error "" "unsigned long int long char" } */
unsigned long int long short *x805; /* { dg-error "" "unsigned long int long short" } */
unsigned long int long int *x806; /* { dg-error "" "unsigned long int long int" } */
unsigned long int long long *x807; /* { dg-error "" "unsigned long int long long" } */
unsigned long int long float *x808; /* { dg-error "" "unsigned long int long float" } */
unsigned long int long double *x809; /* { dg-error "" "unsigned long int long double" } */
unsigned long int long signed *x810; /* { dg-error "" "unsigned long int long signed" } */
unsigned long int long unsigned *x811; /* { dg-error "" "unsigned long int long unsigned" } */
unsigned long long int void *x812; /* { dg-error "" "unsigned long long int void" } */
unsigned long long int char *x813; /* { dg-error "" "unsigned long long int char" } */
unsigned long long int short *x814; /* { dg-error "" "unsigned long long int short" } */
unsigned long long int int *x815; /* { dg-error "" "unsigned long long int int" } */
unsigned long long int long *x816; /* { dg-error "" "unsigned long long int long" } */
unsigned long long int float *x817; /* { dg-error "" "unsigned long long int float" } */
unsigned long long int double *x818; /* { dg-error "" "unsigned long long int double" } */
unsigned long long int signed *x819; /* { dg-error "" "unsigned long long int signed" } */
unsigned long long int unsigned *x820; /* { dg-error "" "unsigned long long int unsigned" } */
