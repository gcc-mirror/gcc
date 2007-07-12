/* Test for valid and invalid combinations of type specifiers in C90.
   Similar to typespec-1.c but with -pedantic-errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

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
long long *x51; /* { dg-error "" "long long" } */
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
int long long *x159; /* { dg-error "" "int long long" } */
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
long int long *x186; /* { dg-error "" "long int long" } */
long int float *x187; /* { dg-error "" "long int float" } */
long int double *x188; /* { dg-error "" "long int double" } */
long int signed *x189;
long int unsigned *x190;
long double void *x191; /* { dg-error "" "long double void" } */
long double char *x192; /* { dg-error "" "long double char" } */
long double short *x193; /* { dg-error "" "long double short" } */
long double int *x194; /* { dg-error "" "long double int" } */
long double long *x195; /* { dg-error "" "long double long" } */
long double float *x196; /* { dg-error "" "long double float" } */
long double double *x197; /* { dg-error "" "long double double" } */
long double signed *x198; /* { dg-error "" "long double signed" } */
long double unsigned *x199; /* { dg-error "" "long double unsigned" } */
long signed void *x200; /* { dg-error "" "long signed void" } */
long signed char *x201; /* { dg-error "" "long signed char" } */
long signed short *x202; /* { dg-error "" "long signed short" } */
long signed int *x203;
long signed long *x204; /* { dg-error "" "long signed long" } */
long signed float *x205; /* { dg-error "" "long signed float" } */
long signed double *x206; /* { dg-error "" "long signed double" } */
long signed signed *x207; /* { dg-error "" "long signed signed" } */
long signed unsigned *x208; /* { dg-error "" "long signed unsigned" } */
long unsigned void *x209; /* { dg-error "" "long unsigned void" } */
long unsigned char *x210; /* { dg-error "" "long unsigned char" } */
long unsigned short *x211; /* { dg-error "" "long unsigned short" } */
long unsigned int *x212;
long unsigned long *x213; /* { dg-error "" "long unsigned long" } */
long unsigned float *x214; /* { dg-error "" "long unsigned float" } */
long unsigned double *x215; /* { dg-error "" "long unsigned double" } */
long unsigned signed *x216; /* { dg-error "" "long unsigned signed" } */
long unsigned unsigned *x217; /* { dg-error "" "long unsigned unsigned" } */
double long void *x218; /* { dg-error "" "double long void" } */
double long char *x219; /* { dg-error "" "double long char" } */
double long short *x220; /* { dg-error "" "double long short" } */
double long int *x221; /* { dg-error "" "double long int" } */
double long long *x222; /* { dg-error "" "double long long" } */
double long float *x223; /* { dg-error "" "double long float" } */
double long double *x224; /* { dg-error "" "double long double" } */
double long signed *x225; /* { dg-error "" "double long signed" } */
double long unsigned *x226; /* { dg-error "" "double long unsigned" } */
signed char void *x227; /* { dg-error "" "signed char void" } */
signed char char *x228; /* { dg-error "" "signed char char" } */
signed char short *x229; /* { dg-error "" "signed char short" } */
signed char int *x230; /* { dg-error "" "signed char int" } */
signed char long *x231; /* { dg-error "" "signed char long" } */
signed char float *x232; /* { dg-error "" "signed char float" } */
signed char double *x233; /* { dg-error "" "signed char double" } */
signed char signed *x234; /* { dg-error "" "signed char signed" } */
signed char unsigned *x235; /* { dg-error "" "signed char unsigned" } */
signed short void *x236; /* { dg-error "" "signed short void" } */
signed short char *x237; /* { dg-error "" "signed short char" } */
signed short short *x238; /* { dg-error "" "signed short short" } */
signed short int *x239;
signed short long *x240; /* { dg-error "" "signed short long" } */
signed short float *x241; /* { dg-error "" "signed short float" } */
signed short double *x242; /* { dg-error "" "signed short double" } */
signed short signed *x243; /* { dg-error "" "signed short signed" } */
signed short unsigned *x244; /* { dg-error "" "signed short unsigned" } */
signed int void *x245; /* { dg-error "" "signed int void" } */
signed int char *x246; /* { dg-error "" "signed int char" } */
signed int short *x247;
signed int int *x248; /* { dg-error "" "signed int int" } */
signed int long *x249;
signed int float *x250; /* { dg-error "" "signed int float" } */
signed int double *x251; /* { dg-error "" "signed int double" } */
signed int signed *x252; /* { dg-error "" "signed int signed" } */
signed int unsigned *x253; /* { dg-error "" "signed int unsigned" } */
signed long void *x254; /* { dg-error "" "signed long void" } */
signed long char *x255; /* { dg-error "" "signed long char" } */
signed long short *x256; /* { dg-error "" "signed long short" } */
signed long int *x257;
signed long long *x258; /* { dg-error "" "signed long long" } */
signed long float *x259; /* { dg-error "" "signed long float" } */
signed long double *x260; /* { dg-error "" "signed long double" } */
signed long signed *x261; /* { dg-error "" "signed long signed" } */
signed long unsigned *x262; /* { dg-error "" "signed long unsigned" } */
unsigned char void *x263; /* { dg-error "" "unsigned char void" } */
unsigned char char *x264; /* { dg-error "" "unsigned char char" } */
unsigned char short *x265; /* { dg-error "" "unsigned char short" } */
unsigned char int *x266; /* { dg-error "" "unsigned char int" } */
unsigned char long *x267; /* { dg-error "" "unsigned char long" } */
unsigned char float *x268; /* { dg-error "" "unsigned char float" } */
unsigned char double *x269; /* { dg-error "" "unsigned char double" } */
unsigned char signed *x270; /* { dg-error "" "unsigned char signed" } */
unsigned char unsigned *x271; /* { dg-error "" "unsigned char unsigned" } */
unsigned short void *x272; /* { dg-error "" "unsigned short void" } */
unsigned short char *x273; /* { dg-error "" "unsigned short char" } */
unsigned short short *x274; /* { dg-error "" "unsigned short short" } */
unsigned short int *x275;
unsigned short long *x276; /* { dg-error "" "unsigned short long" } */
unsigned short float *x277; /* { dg-error "" "unsigned short float" } */
unsigned short double *x278; /* { dg-error "" "unsigned short double" } */
unsigned short signed *x279; /* { dg-error "" "unsigned short signed" } */
unsigned short unsigned *x280; /* { dg-error "" "unsigned short unsigned" } */
unsigned int void *x281; /* { dg-error "" "unsigned int void" } */
unsigned int char *x282; /* { dg-error "" "unsigned int char" } */
unsigned int short *x283;
unsigned int int *x284; /* { dg-error "" "unsigned int int" } */
unsigned int long *x285;
unsigned int float *x286; /* { dg-error "" "unsigned int float" } */
unsigned int double *x287; /* { dg-error "" "unsigned int double" } */
unsigned int signed *x288; /* { dg-error "" "unsigned int signed" } */
unsigned int unsigned *x289; /* { dg-error "" "unsigned int unsigned" } */
unsigned long void *x290; /* { dg-error "" "unsigned long void" } */
unsigned long char *x291; /* { dg-error "" "unsigned long char" } */
unsigned long short *x292; /* { dg-error "" "unsigned long short" } */
unsigned long int *x293;
unsigned long long *x294; /* { dg-error "" "unsigned long long" } */
unsigned long float *x295; /* { dg-error "" "unsigned long float" } */
unsigned long double *x296; /* { dg-error "" "unsigned long double" } */
unsigned long signed *x297; /* { dg-error "" "unsigned long signed" } */
unsigned long unsigned *x298; /* { dg-error "" "unsigned long unsigned" } */
short int signed void *x299; /* { dg-error "" "short int signed void" } */
short int signed char *x300; /* { dg-error "" "short int signed char" } */
short int signed short *x301; /* { dg-error "" "short int signed short" } */
short int signed int *x302; /* { dg-error "" "short int signed int" } */
short int signed long *x303; /* { dg-error "" "short int signed long" } */
short int signed float *x304; /* { dg-error "" "short int signed float" } */
short int signed double *x305; /* { dg-error "" "short int signed double" } */
short int signed signed *x306; /* { dg-error "" "short int signed signed" } */
short int signed unsigned *x307; /* { dg-error "" "short int signed unsigned" } */
short int unsigned void *x308; /* { dg-error "" "short int unsigned void" } */
short int unsigned char *x309; /* { dg-error "" "short int unsigned char" } */
short int unsigned short *x310; /* { dg-error "" "short int unsigned short" } */
short int unsigned int *x311; /* { dg-error "" "short int unsigned int" } */
short int unsigned long *x312; /* { dg-error "" "short int unsigned long" } */
short int unsigned float *x313; /* { dg-error "" "short int unsigned float" } */
short int unsigned double *x314; /* { dg-error "" "short int unsigned double" } */
short int unsigned signed *x315; /* { dg-error "" "short int unsigned signed" } */
short int unsigned unsigned *x316; /* { dg-error "" "short int unsigned unsigned" } */
short signed int void *x317; /* { dg-error "" "short signed int void" } */
short signed int char *x318; /* { dg-error "" "short signed int char" } */
short signed int short *x319; /* { dg-error "" "short signed int short" } */
short signed int int *x320; /* { dg-error "" "short signed int int" } */
short signed int long *x321; /* { dg-error "" "short signed int long" } */
short signed int float *x322; /* { dg-error "" "short signed int float" } */
short signed int double *x323; /* { dg-error "" "short signed int double" } */
short signed int signed *x324; /* { dg-error "" "short signed int signed" } */
short signed int unsigned *x325; /* { dg-error "" "short signed int unsigned" } */
short unsigned int void *x326; /* { dg-error "" "short unsigned int void" } */
short unsigned int char *x327; /* { dg-error "" "short unsigned int char" } */
short unsigned int short *x328; /* { dg-error "" "short unsigned int short" } */
short unsigned int int *x329; /* { dg-error "" "short unsigned int int" } */
short unsigned int long *x330; /* { dg-error "" "short unsigned int long" } */
short unsigned int float *x331; /* { dg-error "" "short unsigned int float" } */
short unsigned int double *x332; /* { dg-error "" "short unsigned int double" } */
short unsigned int signed *x333; /* { dg-error "" "short unsigned int signed" } */
short unsigned int unsigned *x334; /* { dg-error "" "short unsigned int unsigned" } */
int short signed void *x335; /* { dg-error "" "int short signed void" } */
int short signed char *x336; /* { dg-error "" "int short signed char" } */
int short signed short *x337; /* { dg-error "" "int short signed short" } */
int short signed int *x338; /* { dg-error "" "int short signed int" } */
int short signed long *x339; /* { dg-error "" "int short signed long" } */
int short signed float *x340; /* { dg-error "" "int short signed float" } */
int short signed double *x341; /* { dg-error "" "int short signed double" } */
int short signed signed *x342; /* { dg-error "" "int short signed signed" } */
int short signed unsigned *x343; /* { dg-error "" "int short signed unsigned" } */
int short unsigned void *x344; /* { dg-error "" "int short unsigned void" } */
int short unsigned char *x345; /* { dg-error "" "int short unsigned char" } */
int short unsigned short *x346; /* { dg-error "" "int short unsigned short" } */
int short unsigned int *x347; /* { dg-error "" "int short unsigned int" } */
int short unsigned long *x348; /* { dg-error "" "int short unsigned long" } */
int short unsigned float *x349; /* { dg-error "" "int short unsigned float" } */
int short unsigned double *x350; /* { dg-error "" "int short unsigned double" } */
int short unsigned signed *x351; /* { dg-error "" "int short unsigned signed" } */
int short unsigned unsigned *x352; /* { dg-error "" "int short unsigned unsigned" } */
int long signed void *x353; /* { dg-error "" "int long signed void" } */
int long signed char *x354; /* { dg-error "" "int long signed char" } */
int long signed short *x355; /* { dg-error "" "int long signed short" } */
int long signed int *x356; /* { dg-error "" "int long signed int" } */
int long signed long *x357; /* { dg-error "" "int long signed long" } */
int long signed float *x358; /* { dg-error "" "int long signed float" } */
int long signed double *x359; /* { dg-error "" "int long signed double" } */
int long signed signed *x360; /* { dg-error "" "int long signed signed" } */
int long signed unsigned *x361; /* { dg-error "" "int long signed unsigned" } */
int long unsigned void *x362; /* { dg-error "" "int long unsigned void" } */
int long unsigned char *x363; /* { dg-error "" "int long unsigned char" } */
int long unsigned short *x364; /* { dg-error "" "int long unsigned short" } */
int long unsigned int *x365; /* { dg-error "" "int long unsigned int" } */
int long unsigned long *x366; /* { dg-error "" "int long unsigned long" } */
int long unsigned float *x367; /* { dg-error "" "int long unsigned float" } */
int long unsigned double *x368; /* { dg-error "" "int long unsigned double" } */
int long unsigned signed *x369; /* { dg-error "" "int long unsigned signed" } */
int long unsigned unsigned *x370; /* { dg-error "" "int long unsigned unsigned" } */
int signed short void *x371; /* { dg-error "" "int signed short void" } */
int signed short char *x372; /* { dg-error "" "int signed short char" } */
int signed short short *x373; /* { dg-error "" "int signed short short" } */
int signed short int *x374; /* { dg-error "" "int signed short int" } */
int signed short long *x375; /* { dg-error "" "int signed short long" } */
int signed short float *x376; /* { dg-error "" "int signed short float" } */
int signed short double *x377; /* { dg-error "" "int signed short double" } */
int signed short signed *x378; /* { dg-error "" "int signed short signed" } */
int signed short unsigned *x379; /* { dg-error "" "int signed short unsigned" } */
int signed long void *x380; /* { dg-error "" "int signed long void" } */
int signed long char *x381; /* { dg-error "" "int signed long char" } */
int signed long short *x382; /* { dg-error "" "int signed long short" } */
int signed long int *x383; /* { dg-error "" "int signed long int" } */
int signed long long *x384; /* { dg-error "" "int signed long long" } */
int signed long float *x385; /* { dg-error "" "int signed long float" } */
int signed long double *x386; /* { dg-error "" "int signed long double" } */
int signed long signed *x387; /* { dg-error "" "int signed long signed" } */
int signed long unsigned *x388; /* { dg-error "" "int signed long unsigned" } */
int unsigned short void *x389; /* { dg-error "" "int unsigned short void" } */
int unsigned short char *x390; /* { dg-error "" "int unsigned short char" } */
int unsigned short short *x391; /* { dg-error "" "int unsigned short short" } */
int unsigned short int *x392; /* { dg-error "" "int unsigned short int" } */
int unsigned short long *x393; /* { dg-error "" "int unsigned short long" } */
int unsigned short float *x394; /* { dg-error "" "int unsigned short float" } */
int unsigned short double *x395; /* { dg-error "" "int unsigned short double" } */
int unsigned short signed *x396; /* { dg-error "" "int unsigned short signed" } */
int unsigned short unsigned *x397; /* { dg-error "" "int unsigned short unsigned" } */
int unsigned long void *x398; /* { dg-error "" "int unsigned long void" } */
int unsigned long char *x399; /* { dg-error "" "int unsigned long char" } */
int unsigned long short *x400; /* { dg-error "" "int unsigned long short" } */
int unsigned long int *x401; /* { dg-error "" "int unsigned long int" } */
int unsigned long long *x402; /* { dg-error "" "int unsigned long long" } */
int unsigned long float *x403; /* { dg-error "" "int unsigned long float" } */
int unsigned long double *x404; /* { dg-error "" "int unsigned long double" } */
int unsigned long signed *x405; /* { dg-error "" "int unsigned long signed" } */
int unsigned long unsigned *x406; /* { dg-error "" "int unsigned long unsigned" } */
long int signed void *x407; /* { dg-error "" "long int signed void" } */
long int signed char *x408; /* { dg-error "" "long int signed char" } */
long int signed short *x409; /* { dg-error "" "long int signed short" } */
long int signed int *x410; /* { dg-error "" "long int signed int" } */
long int signed long *x411; /* { dg-error "" "long int signed long" } */
long int signed float *x412; /* { dg-error "" "long int signed float" } */
long int signed double *x413; /* { dg-error "" "long int signed double" } */
long int signed signed *x414; /* { dg-error "" "long int signed signed" } */
long int signed unsigned *x415; /* { dg-error "" "long int signed unsigned" } */
long int unsigned void *x416; /* { dg-error "" "long int unsigned void" } */
long int unsigned char *x417; /* { dg-error "" "long int unsigned char" } */
long int unsigned short *x418; /* { dg-error "" "long int unsigned short" } */
long int unsigned int *x419; /* { dg-error "" "long int unsigned int" } */
long int unsigned long *x420; /* { dg-error "" "long int unsigned long" } */
long int unsigned float *x421; /* { dg-error "" "long int unsigned float" } */
long int unsigned double *x422; /* { dg-error "" "long int unsigned double" } */
long int unsigned signed *x423; /* { dg-error "" "long int unsigned signed" } */
long int unsigned unsigned *x424; /* { dg-error "" "long int unsigned unsigned" } */
long signed int void *x425; /* { dg-error "" "long signed int void" } */
long signed int char *x426; /* { dg-error "" "long signed int char" } */
long signed int short *x427; /* { dg-error "" "long signed int short" } */
long signed int int *x428; /* { dg-error "" "long signed int int" } */
long signed int long *x429; /* { dg-error "" "long signed int long" } */
long signed int float *x430; /* { dg-error "" "long signed int float" } */
long signed int double *x431; /* { dg-error "" "long signed int double" } */
long signed int signed *x432; /* { dg-error "" "long signed int signed" } */
long signed int unsigned *x433; /* { dg-error "" "long signed int unsigned" } */
long unsigned int void *x434; /* { dg-error "" "long unsigned int void" } */
long unsigned int char *x435; /* { dg-error "" "long unsigned int char" } */
long unsigned int short *x436; /* { dg-error "" "long unsigned int short" } */
long unsigned int int *x437; /* { dg-error "" "long unsigned int int" } */
long unsigned int long *x438; /* { dg-error "" "long unsigned int long" } */
long unsigned int float *x439; /* { dg-error "" "long unsigned int float" } */
long unsigned int double *x440; /* { dg-error "" "long unsigned int double" } */
long unsigned int signed *x441; /* { dg-error "" "long unsigned int signed" } */
long unsigned int unsigned *x442; /* { dg-error "" "long unsigned int unsigned" } */
signed short int void *x443; /* { dg-error "" "signed short int void" } */
signed short int char *x444; /* { dg-error "" "signed short int char" } */
signed short int short *x445; /* { dg-error "" "signed short int short" } */
signed short int int *x446; /* { dg-error "" "signed short int int" } */
signed short int long *x447; /* { dg-error "" "signed short int long" } */
signed short int float *x448; /* { dg-error "" "signed short int float" } */
signed short int double *x449; /* { dg-error "" "signed short int double" } */
signed short int signed *x450; /* { dg-error "" "signed short int signed" } */
signed short int unsigned *x451; /* { dg-error "" "signed short int unsigned" } */
signed int short void *x452; /* { dg-error "" "signed int short void" } */
signed int short char *x453; /* { dg-error "" "signed int short char" } */
signed int short short *x454; /* { dg-error "" "signed int short short" } */
signed int short int *x455; /* { dg-error "" "signed int short int" } */
signed int short long *x456; /* { dg-error "" "signed int short long" } */
signed int short float *x457; /* { dg-error "" "signed int short float" } */
signed int short double *x458; /* { dg-error "" "signed int short double" } */
signed int short signed *x459; /* { dg-error "" "signed int short signed" } */
signed int short unsigned *x460; /* { dg-error "" "signed int short unsigned" } */
signed int long void *x461; /* { dg-error "" "signed int long void" } */
signed int long char *x462; /* { dg-error "" "signed int long char" } */
signed int long short *x463; /* { dg-error "" "signed int long short" } */
signed int long int *x464; /* { dg-error "" "signed int long int" } */
signed int long long *x465; /* { dg-error "" "signed int long long" } */
signed int long float *x466; /* { dg-error "" "signed int long float" } */
signed int long double *x467; /* { dg-error "" "signed int long double" } */
signed int long signed *x468; /* { dg-error "" "signed int long signed" } */
signed int long unsigned *x469; /* { dg-error "" "signed int long unsigned" } */
signed long int void *x470; /* { dg-error "" "signed long int void" } */
signed long int char *x471; /* { dg-error "" "signed long int char" } */
signed long int short *x472; /* { dg-error "" "signed long int short" } */
signed long int int *x473; /* { dg-error "" "signed long int int" } */
signed long int long *x474; /* { dg-error "" "signed long int long" } */
signed long int float *x475; /* { dg-error "" "signed long int float" } */
signed long int double *x476; /* { dg-error "" "signed long int double" } */
signed long int signed *x477; /* { dg-error "" "signed long int signed" } */
signed long int unsigned *x478; /* { dg-error "" "signed long int unsigned" } */
unsigned short int void *x479; /* { dg-error "" "unsigned short int void" } */
unsigned short int char *x480; /* { dg-error "" "unsigned short int char" } */
unsigned short int short *x481; /* { dg-error "" "unsigned short int short" } */
unsigned short int int *x482; /* { dg-error "" "unsigned short int int" } */
unsigned short int long *x483; /* { dg-error "" "unsigned short int long" } */
unsigned short int float *x484; /* { dg-error "" "unsigned short int float" } */
unsigned short int double *x485; /* { dg-error "" "unsigned short int double" } */
unsigned short int signed *x486; /* { dg-error "" "unsigned short int signed" } */
unsigned short int unsigned *x487; /* { dg-error "" "unsigned short int unsigned" } */
unsigned int short void *x488; /* { dg-error "" "unsigned int short void" } */
unsigned int short char *x489; /* { dg-error "" "unsigned int short char" } */
unsigned int short short *x490; /* { dg-error "" "unsigned int short short" } */
unsigned int short int *x491; /* { dg-error "" "unsigned int short int" } */
unsigned int short long *x492; /* { dg-error "" "unsigned int short long" } */
unsigned int short float *x493; /* { dg-error "" "unsigned int short float" } */
unsigned int short double *x494; /* { dg-error "" "unsigned int short double" } */
unsigned int short signed *x495; /* { dg-error "" "unsigned int short signed" } */
unsigned int short unsigned *x496; /* { dg-error "" "unsigned int short unsigned" } */
unsigned int long void *x497; /* { dg-error "" "unsigned int long void" } */
unsigned int long char *x498; /* { dg-error "" "unsigned int long char" } */
unsigned int long short *x499; /* { dg-error "" "unsigned int long short" } */
unsigned int long int *x500; /* { dg-error "" "unsigned int long int" } */
unsigned int long long *x501; /* { dg-error "" "unsigned int long long" } */
unsigned int long float *x502; /* { dg-error "" "unsigned int long float" } */
unsigned int long double *x503; /* { dg-error "" "unsigned int long double" } */
unsigned int long signed *x504; /* { dg-error "" "unsigned int long signed" } */
unsigned int long unsigned *x505; /* { dg-error "" "unsigned int long unsigned" } */
unsigned long int void *x506; /* { dg-error "" "unsigned long int void" } */
unsigned long int char *x507; /* { dg-error "" "unsigned long int char" } */
unsigned long int short *x508; /* { dg-error "" "unsigned long int short" } */
unsigned long int int *x509; /* { dg-error "" "unsigned long int int" } */
unsigned long int long *x510; /* { dg-error "" "unsigned long int long" } */
unsigned long int float *x511; /* { dg-error "" "unsigned long int float" } */
unsigned long int double *x512; /* { dg-error "" "unsigned long int double" } */
unsigned long int signed *x513; /* { dg-error "" "unsigned long int signed" } */
unsigned long int unsigned *x514; /* { dg-error "" "unsigned long int unsigned" } */
