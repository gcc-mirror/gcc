#include <stdio.h>
#include <stddef.h>

static struct {} s0;
static union {} u0;
static struct {int :0;} s1;
static union {int :0;} u1;
static struct {char m;} s2;
static union {char m;} u2;
static struct {short m;} s3;
static union {short m;} u3;
static struct {int m;} s4;
static union {int m;} u4;
static struct {long m;} s5;
static union {long m;} u5;
static struct {long long m;} s6;
static union {long long m;} u6;
static struct {char * m;} s7;
static union {char * m;} u7;
static struct {float m;} s8;
static union {float m;} u8;
static struct {double m;} s9;
static union {double m;} u9;
static struct {long double m;} s10;
static union {long double m;} u10;
static struct {ptrdiff_t m;} s11;
static union {ptrdiff_t m;} u11;
static struct {size_t m;} s12;
static union {size_t m;} u12;
static struct {wchar_t m;} s13;
static union {wchar_t m;} u13;
static struct {struct {char m;}m;} s14;
static union {struct {char m;}m;} u14;
static struct {union {char m;}m;} s15;
static union {union {char m;}m;} u15;
static struct {struct {short m;}m;} s16;
static union {struct {short m;}m;} u16;
static struct {union {short m;}m;} s17;
static union {union {short m;}m;} u17;
static struct {struct {int m;}m;} s18;
static union {struct {int m;}m;} u18;
static struct {union {int m;}m;} s19;
static union {union {int m;}m;} u19;
static struct {struct {long m;}m;} s20;
static union {struct {long m;}m;} u20;
static struct {union {long m;}m;} s21;
static union {union {long m;}m;} u21;
static struct {struct {long long m;}m;} s22;
static union {struct {long long m;}m;} u22;
static struct {union {long long m;}m;} s23;
static union {union {long long m;}m;} u23;
static struct {struct {char * m;}m;} s24;
static union {struct {char * m;}m;} u24;
static struct {union {char * m;}m;} s25;
static union {union {char * m;}m;} u25;
static struct {struct {float m;}m;} s26;
static union {struct {float m;}m;} u26;
static struct {union {float m;}m;} s27;
static union {union {float m;}m;} u27;
static struct {struct {double m;}m;} s28;
static union {struct {double m;}m;} u28;
static struct {union {double m;}m;} s29;
static union {union {double m;}m;} u29;
static struct {struct {long double m;}m;} s30;
static union {struct {long double m;}m;} u30;
static struct {union {long double m;}m;} s31;
static union {union {long double m;}m;} u31;
static struct {struct {ptrdiff_t m;}m;} s32;
static union {struct {ptrdiff_t m;}m;} u32;
static struct {union {ptrdiff_t m;}m;} s33;
static union {union {ptrdiff_t m;}m;} u33;
static struct {struct {size_t m;}m;} s34;
static union {struct {size_t m;}m;} u34;
static struct {union {size_t m;}m;} s35;
static union {union {size_t m;}m;} u35;
static struct {struct {wchar_t m;}m;} s36;
static union {struct {wchar_t m;}m;} u36;
static struct {union {wchar_t m;}m;} s37;
static union {union {wchar_t m;}m;} u37;
static struct {struct {}m;} s38;
static union {struct {}m;} u38;
static struct {union {}m;} s39;
static union {union {}m;} u39;
static struct {struct {int :0;}m;} s40;
static union {struct {int :0;}m;} u40;
static struct {union {int :0;}m;} s41;
static union {union {int :0;}m;} u41;
static struct {char m [0];} s42;
static union {char m [0];} u42;
static struct {short m [0];} s43;
static union {short m [0];} u43;
static struct {int m [0];} s44;
static union {int m [0];} u44;
static struct {long m [0];} s45;
static union {long m [0];} u45;
static struct {long long m [0];} s46;
static union {long long m [0];} u46;
static struct {char * m [0];} s47;
static union {char * m [0];} u47;
static struct {float m [0];} s48;
static union {float m [0];} u48;
static struct {double m [0];} s49;
static union {double m [0];} u49;
static struct {long double m [0];} s50;
static union {long double m [0];} u50;
static struct {ptrdiff_t m [0];} s51;
static union {ptrdiff_t m [0];} u51;
static struct {size_t m [0];} s52;
static union {size_t m [0];} u52;
static struct {wchar_t m [0];} s53;
static union {wchar_t m [0];} u53;
int main (void) {
  int min_size = 0;
  int min_align = 0;
  printf ("+++Minimal struct size/alignment:\n");
  if (min_size > sizeof (s0))
     min_size = sizeof (s0);
  if (min_align > __alignof__ (s0))
     min_align = __alignof__ (s0);
  if (min_size > sizeof (u0))
     min_size = sizeof (u0);
  if (min_align > __alignof__ (u0))
     min_align = __alignof__ (u0);
  if (min_size > sizeof (s1))
     min_size = sizeof (s1);
  if (min_align > __alignof__ (s1))
     min_align = __alignof__ (s1);
  if (min_size > sizeof (u1))
     min_size = sizeof (u1);
  if (min_align > __alignof__ (u1))
     min_align = __alignof__ (u1);
  if (min_size > sizeof (s2))
     min_size = sizeof (s2);
  if (min_align > __alignof__ (s2))
     min_align = __alignof__ (s2);
  if (min_size > sizeof (u2))
     min_size = sizeof (u2);
  if (min_align > __alignof__ (u2))
     min_align = __alignof__ (u2);
  if (min_size > sizeof (s3))
     min_size = sizeof (s3);
  if (min_align > __alignof__ (s3))
     min_align = __alignof__ (s3);
  if (min_size > sizeof (u3))
     min_size = sizeof (u3);
  if (min_align > __alignof__ (u3))
     min_align = __alignof__ (u3);
  if (min_size > sizeof (s4))
     min_size = sizeof (s4);
  if (min_align > __alignof__ (s4))
     min_align = __alignof__ (s4);
  if (min_size > sizeof (u4))
     min_size = sizeof (u4);
  if (min_align > __alignof__ (u4))
     min_align = __alignof__ (u4);
  if (min_size > sizeof (s5))
     min_size = sizeof (s5);
  if (min_align > __alignof__ (s5))
     min_align = __alignof__ (s5);
  if (min_size > sizeof (u5))
     min_size = sizeof (u5);
  if (min_align > __alignof__ (u5))
     min_align = __alignof__ (u5);
  if (min_size > sizeof (s6))
     min_size = sizeof (s6);
  if (min_align > __alignof__ (s6))
     min_align = __alignof__ (s6);
  if (min_size > sizeof (u6))
     min_size = sizeof (u6);
  if (min_align > __alignof__ (u6))
     min_align = __alignof__ (u6);
  if (min_size > sizeof (s7))
     min_size = sizeof (s7);
  if (min_align > __alignof__ (s7))
     min_align = __alignof__ (s7);
  if (min_size > sizeof (u7))
     min_size = sizeof (u7);
  if (min_align > __alignof__ (u7))
     min_align = __alignof__ (u7);
  if (min_size > sizeof (s8))
     min_size = sizeof (s8);
  if (min_align > __alignof__ (s8))
     min_align = __alignof__ (s8);
  if (min_size > sizeof (u8))
     min_size = sizeof (u8);
  if (min_align > __alignof__ (u8))
     min_align = __alignof__ (u8);
  if (min_size > sizeof (s9))
     min_size = sizeof (s9);
  if (min_align > __alignof__ (s9))
     min_align = __alignof__ (s9);
  if (min_size > sizeof (u9))
     min_size = sizeof (u9);
  if (min_align > __alignof__ (u9))
     min_align = __alignof__ (u9);
  if (min_size > sizeof (s10))
     min_size = sizeof (s10);
  if (min_align > __alignof__ (s10))
     min_align = __alignof__ (s10);
  if (min_size > sizeof (u10))
     min_size = sizeof (u10);
  if (min_align > __alignof__ (u10))
     min_align = __alignof__ (u10);
  if (min_size > sizeof (s11))
     min_size = sizeof (s11);
  if (min_align > __alignof__ (s11))
     min_align = __alignof__ (s11);
  if (min_size > sizeof (u11))
     min_size = sizeof (u11);
  if (min_align > __alignof__ (u11))
     min_align = __alignof__ (u11);
  if (min_size > sizeof (s12))
     min_size = sizeof (s12);
  if (min_align > __alignof__ (s12))
     min_align = __alignof__ (s12);
  if (min_size > sizeof (u12))
     min_size = sizeof (u12);
  if (min_align > __alignof__ (u12))
     min_align = __alignof__ (u12);
  if (min_size > sizeof (s13))
     min_size = sizeof (s13);
  if (min_align > __alignof__ (s13))
     min_align = __alignof__ (s13);
  if (min_size > sizeof (u13))
     min_size = sizeof (u13);
  if (min_align > __alignof__ (u13))
     min_align = __alignof__ (u13);
  if (min_size > sizeof (s14))
     min_size = sizeof (s14);
  if (min_align > __alignof__ (s14))
     min_align = __alignof__ (s14);
  if (min_size > sizeof (u14))
     min_size = sizeof (u14);
  if (min_align > __alignof__ (u14))
     min_align = __alignof__ (u14);
  if (min_size > sizeof (s15))
     min_size = sizeof (s15);
  if (min_align > __alignof__ (s15))
     min_align = __alignof__ (s15);
  if (min_size > sizeof (u15))
     min_size = sizeof (u15);
  if (min_align > __alignof__ (u15))
     min_align = __alignof__ (u15);
  if (min_size > sizeof (s16))
     min_size = sizeof (s16);
  if (min_align > __alignof__ (s16))
     min_align = __alignof__ (s16);
  if (min_size > sizeof (u16))
     min_size = sizeof (u16);
  if (min_align > __alignof__ (u16))
     min_align = __alignof__ (u16);
  if (min_size > sizeof (s17))
     min_size = sizeof (s17);
  if (min_align > __alignof__ (s17))
     min_align = __alignof__ (s17);
  if (min_size > sizeof (u17))
     min_size = sizeof (u17);
  if (min_align > __alignof__ (u17))
     min_align = __alignof__ (u17);
  if (min_size > sizeof (s18))
     min_size = sizeof (s18);
  if (min_align > __alignof__ (s18))
     min_align = __alignof__ (s18);
  if (min_size > sizeof (u18))
     min_size = sizeof (u18);
  if (min_align > __alignof__ (u18))
     min_align = __alignof__ (u18);
  if (min_size > sizeof (s19))
     min_size = sizeof (s19);
  if (min_align > __alignof__ (s19))
     min_align = __alignof__ (s19);
  if (min_size > sizeof (u19))
     min_size = sizeof (u19);
  if (min_align > __alignof__ (u19))
     min_align = __alignof__ (u19);
  if (min_size > sizeof (s20))
     min_size = sizeof (s20);
  if (min_align > __alignof__ (s20))
     min_align = __alignof__ (s20);
  if (min_size > sizeof (u20))
     min_size = sizeof (u20);
  if (min_align > __alignof__ (u20))
     min_align = __alignof__ (u20);
  if (min_size > sizeof (s21))
     min_size = sizeof (s21);
  if (min_align > __alignof__ (s21))
     min_align = __alignof__ (s21);
  if (min_size > sizeof (u21))
     min_size = sizeof (u21);
  if (min_align > __alignof__ (u21))
     min_align = __alignof__ (u21);
  if (min_size > sizeof (s22))
     min_size = sizeof (s22);
  if (min_align > __alignof__ (s22))
     min_align = __alignof__ (s22);
  if (min_size > sizeof (u22))
     min_size = sizeof (u22);
  if (min_align > __alignof__ (u22))
     min_align = __alignof__ (u22);
  if (min_size > sizeof (s23))
     min_size = sizeof (s23);
  if (min_align > __alignof__ (s23))
     min_align = __alignof__ (s23);
  if (min_size > sizeof (u23))
     min_size = sizeof (u23);
  if (min_align > __alignof__ (u23))
     min_align = __alignof__ (u23);
  if (min_size > sizeof (s24))
     min_size = sizeof (s24);
  if (min_align > __alignof__ (s24))
     min_align = __alignof__ (s24);
  if (min_size > sizeof (u24))
     min_size = sizeof (u24);
  if (min_align > __alignof__ (u24))
     min_align = __alignof__ (u24);
  if (min_size > sizeof (s25))
     min_size = sizeof (s25);
  if (min_align > __alignof__ (s25))
     min_align = __alignof__ (s25);
  if (min_size > sizeof (u25))
     min_size = sizeof (u25);
  if (min_align > __alignof__ (u25))
     min_align = __alignof__ (u25);
  if (min_size > sizeof (s26))
     min_size = sizeof (s26);
  if (min_align > __alignof__ (s26))
     min_align = __alignof__ (s26);
  if (min_size > sizeof (u26))
     min_size = sizeof (u26);
  if (min_align > __alignof__ (u26))
     min_align = __alignof__ (u26);
  if (min_size > sizeof (s27))
     min_size = sizeof (s27);
  if (min_align > __alignof__ (s27))
     min_align = __alignof__ (s27);
  if (min_size > sizeof (u27))
     min_size = sizeof (u27);
  if (min_align > __alignof__ (u27))
     min_align = __alignof__ (u27);
  if (min_size > sizeof (s28))
     min_size = sizeof (s28);
  if (min_align > __alignof__ (s28))
     min_align = __alignof__ (s28);
  if (min_size > sizeof (u28))
     min_size = sizeof (u28);
  if (min_align > __alignof__ (u28))
     min_align = __alignof__ (u28);
  if (min_size > sizeof (s29))
     min_size = sizeof (s29);
  if (min_align > __alignof__ (s29))
     min_align = __alignof__ (s29);
  if (min_size > sizeof (u29))
     min_size = sizeof (u29);
  if (min_align > __alignof__ (u29))
     min_align = __alignof__ (u29);
  if (min_size > sizeof (s30))
     min_size = sizeof (s30);
  if (min_align > __alignof__ (s30))
     min_align = __alignof__ (s30);
  if (min_size > sizeof (u30))
     min_size = sizeof (u30);
  if (min_align > __alignof__ (u30))
     min_align = __alignof__ (u30);
  if (min_size > sizeof (s31))
     min_size = sizeof (s31);
  if (min_align > __alignof__ (s31))
     min_align = __alignof__ (s31);
  if (min_size > sizeof (u31))
     min_size = sizeof (u31);
  if (min_align > __alignof__ (u31))
     min_align = __alignof__ (u31);
  if (min_size > sizeof (s32))
     min_size = sizeof (s32);
  if (min_align > __alignof__ (s32))
     min_align = __alignof__ (s32);
  if (min_size > sizeof (u32))
     min_size = sizeof (u32);
  if (min_align > __alignof__ (u32))
     min_align = __alignof__ (u32);
  if (min_size > sizeof (s33))
     min_size = sizeof (s33);
  if (min_align > __alignof__ (s33))
     min_align = __alignof__ (s33);
  if (min_size > sizeof (u33))
     min_size = sizeof (u33);
  if (min_align > __alignof__ (u33))
     min_align = __alignof__ (u33);
  if (min_size > sizeof (s34))
     min_size = sizeof (s34);
  if (min_align > __alignof__ (s34))
     min_align = __alignof__ (s34);
  if (min_size > sizeof (u34))
     min_size = sizeof (u34);
  if (min_align > __alignof__ (u34))
     min_align = __alignof__ (u34);
  if (min_size > sizeof (s35))
     min_size = sizeof (s35);
  if (min_align > __alignof__ (s35))
     min_align = __alignof__ (s35);
  if (min_size > sizeof (u35))
     min_size = sizeof (u35);
  if (min_align > __alignof__ (u35))
     min_align = __alignof__ (u35);
  if (min_size > sizeof (s36))
     min_size = sizeof (s36);
  if (min_align > __alignof__ (s36))
     min_align = __alignof__ (s36);
  if (min_size > sizeof (u36))
     min_size = sizeof (u36);
  if (min_align > __alignof__ (u36))
     min_align = __alignof__ (u36);
  if (min_size > sizeof (s37))
     min_size = sizeof (s37);
  if (min_align > __alignof__ (s37))
     min_align = __alignof__ (s37);
  if (min_size > sizeof (u37))
     min_size = sizeof (u37);
  if (min_align > __alignof__ (u37))
     min_align = __alignof__ (u37);
  if (min_size > sizeof (s38))
     min_size = sizeof (s38);
  if (min_align > __alignof__ (s38))
     min_align = __alignof__ (s38);
  if (min_size > sizeof (u38))
     min_size = sizeof (u38);
  if (min_align > __alignof__ (u38))
     min_align = __alignof__ (u38);
  if (min_size > sizeof (s39))
     min_size = sizeof (s39);
  if (min_align > __alignof__ (s39))
     min_align = __alignof__ (s39);
  if (min_size > sizeof (u39))
     min_size = sizeof (u39);
  if (min_align > __alignof__ (u39))
     min_align = __alignof__ (u39);
  if (min_size > sizeof (s40))
     min_size = sizeof (s40);
  if (min_align > __alignof__ (s40))
     min_align = __alignof__ (s40);
  if (min_size > sizeof (u40))
     min_size = sizeof (u40);
  if (min_align > __alignof__ (u40))
     min_align = __alignof__ (u40);
  if (min_size > sizeof (s41))
     min_size = sizeof (s41);
  if (min_align > __alignof__ (s41))
     min_align = __alignof__ (s41);
  if (min_size > sizeof (u41))
     min_size = sizeof (u41);
  if (min_align > __alignof__ (u41))
     min_align = __alignof__ (u41);
  if (min_size > sizeof (s42))
     min_size = sizeof (s42);
  if (min_align > __alignof__ (s42))
     min_align = __alignof__ (s42);
  if (min_size > sizeof (u42))
     min_size = sizeof (u42);
  if (min_align > __alignof__ (u42))
     min_align = __alignof__ (u42);
  if (min_size > sizeof (s43))
     min_size = sizeof (s43);
  if (min_align > __alignof__ (s43))
     min_align = __alignof__ (s43);
  if (min_size > sizeof (u43))
     min_size = sizeof (u43);
  if (min_align > __alignof__ (u43))
     min_align = __alignof__ (u43);
  if (min_size > sizeof (s44))
     min_size = sizeof (s44);
  if (min_align > __alignof__ (s44))
     min_align = __alignof__ (s44);
  if (min_size > sizeof (u44))
     min_size = sizeof (u44);
  if (min_align > __alignof__ (u44))
     min_align = __alignof__ (u44);
  if (min_size > sizeof (s45))
     min_size = sizeof (s45);
  if (min_align > __alignof__ (s45))
     min_align = __alignof__ (s45);
  if (min_size > sizeof (u45))
     min_size = sizeof (u45);
  if (min_align > __alignof__ (u45))
     min_align = __alignof__ (u45);
  if (min_size > sizeof (s46))
     min_size = sizeof (s46);
  if (min_align > __alignof__ (s46))
     min_align = __alignof__ (s46);
  if (min_size > sizeof (u46))
     min_size = sizeof (u46);
  if (min_align > __alignof__ (u46))
     min_align = __alignof__ (u46);
  if (min_size > sizeof (s47))
     min_size = sizeof (s47);
  if (min_align > __alignof__ (s47))
     min_align = __alignof__ (s47);
  if (min_size > sizeof (u47))
     min_size = sizeof (u47);
  if (min_align > __alignof__ (u47))
     min_align = __alignof__ (u47);
  if (min_size > sizeof (s48))
     min_size = sizeof (s48);
  if (min_align > __alignof__ (s48))
     min_align = __alignof__ (s48);
  if (min_size > sizeof (u48))
     min_size = sizeof (u48);
  if (min_align > __alignof__ (u48))
     min_align = __alignof__ (u48);
  if (min_size > sizeof (s49))
     min_size = sizeof (s49);
  if (min_align > __alignof__ (s49))
     min_align = __alignof__ (s49);
  if (min_size > sizeof (u49))
     min_size = sizeof (u49);
  if (min_align > __alignof__ (u49))
     min_align = __alignof__ (u49);
  if (min_size > sizeof (s50))
     min_size = sizeof (s50);
  if (min_align > __alignof__ (s50))
     min_align = __alignof__ (s50);
  if (min_size > sizeof (u50))
     min_size = sizeof (u50);
  if (min_align > __alignof__ (u50))
     min_align = __alignof__ (u50);
  if (min_size > sizeof (s51))
     min_size = sizeof (s51);
  if (min_align > __alignof__ (s51))
     min_align = __alignof__ (s51);
  if (min_size > sizeof (u51))
     min_size = sizeof (u51);
  if (min_align > __alignof__ (u51))
     min_align = __alignof__ (u51);
  if (min_size > sizeof (s52))
     min_size = sizeof (s52);
  if (min_align > __alignof__ (s52))
     min_align = __alignof__ (s52);
  if (min_size > sizeof (u52))
     min_size = sizeof (u52);
  if (min_align > __alignof__ (u52))
     min_align = __alignof__ (u52);
  if (min_size > sizeof (s53))
     min_size = sizeof (s53);
  if (min_align > __alignof__ (s53))
     min_align = __alignof__ (s53);
  if (min_size > sizeof (u53))
     min_size = sizeof (u53);
  if (min_align > __alignof__ (u53))
     min_align = __alignof__ (u53);
  printf ("min struct/union size =%d\n", min_size);
  printf ("min struct/union align=%d\n", min_align);
  return 0;
}
