#include <stdio.h>
#include <stddef.h>

static struct {char m;} s0;
static struct {short m;} s1;
static struct {int m;} s2;
static struct {long m;} s3;
static struct {long long m;} s4;
static struct {char * m;} s5;
static struct {float m;} s6;
static struct {double m;} s7;
static struct {long double m;} s8;
static struct {ptrdiff_t m;} s9;
static struct {size_t m;} s10;
static struct {wchar_t m;} s11;
static struct {struct {char m;}m;} s12;
static struct {struct {short m;}m;} s13;
static struct {struct {int m;}m;} s14;
static struct {struct {long m;}m;} s15;
static struct {struct {long long m;}m;} s16;
static struct {struct {char * m;}m;} s17;
static struct {struct {float m;}m;} s18;
static struct {struct {double m;}m;} s19;
static struct {struct {long double m;}m;} s20;
static struct {struct {ptrdiff_t m;}m;} s21;
static struct {struct {size_t m;}m;} s22;
static struct {struct {wchar_t m;}m;} s23;
static struct {char m [10];} s24;
static struct {short m [10];} s25;
static struct {int m [10];} s26;
static struct {long m [10];} s27;
static struct {long long m [10];} s28;
static struct {char * m [10];} s29;
static struct {float m [10];} s30;
static struct {double m [10];} s31;
static struct {long double m [10];} s32;
static struct {ptrdiff_t m [10];} s33;
static struct {size_t m [10];} s34;
static struct {wchar_t m [10];} s35;
static struct {int :0; char m;} s36;
static struct {int :0; short m;} s37;
static struct {int :0; int m;} s38;
static struct {int :0; long m;} s39;
static struct {int :0; long long m;} s40;
static struct {int :0; char * m;} s41;
static struct {int :0; float m;} s42;
static struct {int :0; double m;} s43;
static struct {int :0; long double m;} s44;
static struct {int :0; ptrdiff_t m;} s45;
static struct {int :0; size_t m;} s46;
static struct {int :0; wchar_t m;} s47;
int main (void) {
  int max_align = 0;
  printf ("+++Biggest field alignment:\n");
  if (max_align < __alignof__ (s0.m))
     max_align = __alignof__ (s0.m);
  if (max_align < __alignof__ (s1.m))
     max_align = __alignof__ (s1.m);
  if (max_align < __alignof__ (s2.m))
     max_align = __alignof__ (s2.m);
  if (max_align < __alignof__ (s3.m))
     max_align = __alignof__ (s3.m);
  if (max_align < __alignof__ (s4.m))
     max_align = __alignof__ (s4.m);
  if (max_align < __alignof__ (s5.m))
     max_align = __alignof__ (s5.m);
  if (max_align < __alignof__ (s6.m))
     max_align = __alignof__ (s6.m);
  if (max_align < __alignof__ (s7.m))
     max_align = __alignof__ (s7.m);
  if (max_align < __alignof__ (s8.m))
     max_align = __alignof__ (s8.m);
  if (max_align < __alignof__ (s9.m))
     max_align = __alignof__ (s9.m);
  if (max_align < __alignof__ (s10.m))
     max_align = __alignof__ (s10.m);
  if (max_align < __alignof__ (s11.m))
     max_align = __alignof__ (s11.m);
  if (max_align < __alignof__ (s12.m))
     max_align = __alignof__ (s12.m);
  if (max_align < __alignof__ (s13.m))
     max_align = __alignof__ (s13.m);
  if (max_align < __alignof__ (s14.m))
     max_align = __alignof__ (s14.m);
  if (max_align < __alignof__ (s15.m))
     max_align = __alignof__ (s15.m);
  if (max_align < __alignof__ (s16.m))
     max_align = __alignof__ (s16.m);
  if (max_align < __alignof__ (s17.m))
     max_align = __alignof__ (s17.m);
  if (max_align < __alignof__ (s18.m))
     max_align = __alignof__ (s18.m);
  if (max_align < __alignof__ (s19.m))
     max_align = __alignof__ (s19.m);
  if (max_align < __alignof__ (s20.m))
     max_align = __alignof__ (s20.m);
  if (max_align < __alignof__ (s21.m))
     max_align = __alignof__ (s21.m);
  if (max_align < __alignof__ (s22.m))
     max_align = __alignof__ (s22.m);
  if (max_align < __alignof__ (s23.m))
     max_align = __alignof__ (s23.m);
  if (max_align < __alignof__ (s24.m))
     max_align = __alignof__ (s24.m);
  if (max_align < __alignof__ (s25.m))
     max_align = __alignof__ (s25.m);
  if (max_align < __alignof__ (s26.m))
     max_align = __alignof__ (s26.m);
  if (max_align < __alignof__ (s27.m))
     max_align = __alignof__ (s27.m);
  if (max_align < __alignof__ (s28.m))
     max_align = __alignof__ (s28.m);
  if (max_align < __alignof__ (s29.m))
     max_align = __alignof__ (s29.m);
  if (max_align < __alignof__ (s30.m))
     max_align = __alignof__ (s30.m);
  if (max_align < __alignof__ (s31.m))
     max_align = __alignof__ (s31.m);
  if (max_align < __alignof__ (s32.m))
     max_align = __alignof__ (s32.m);
  if (max_align < __alignof__ (s33.m))
     max_align = __alignof__ (s33.m);
  if (max_align < __alignof__ (s34.m))
     max_align = __alignof__ (s34.m);
  if (max_align < __alignof__ (s35.m))
     max_align = __alignof__ (s35.m);
  if (max_align < __alignof__ (s36.m))
     max_align = __alignof__ (s36.m);
  if (max_align < __alignof__ (s37.m))
     max_align = __alignof__ (s37.m);
  if (max_align < __alignof__ (s38.m))
     max_align = __alignof__ (s38.m);
  if (max_align < __alignof__ (s39.m))
     max_align = __alignof__ (s39.m);
  if (max_align < __alignof__ (s40.m))
     max_align = __alignof__ (s40.m);
  if (max_align < __alignof__ (s41.m))
     max_align = __alignof__ (s41.m);
  if (max_align < __alignof__ (s42.m))
     max_align = __alignof__ (s42.m);
  if (max_align < __alignof__ (s43.m))
     max_align = __alignof__ (s43.m);
  if (max_align < __alignof__ (s44.m))
     max_align = __alignof__ (s44.m);
  if (max_align < __alignof__ (s45.m))
     max_align = __alignof__ (s45.m);
  if (max_align < __alignof__ (s46.m))
     max_align = __alignof__ (s46.m);
  if (max_align < __alignof__ (s47.m))
     max_align = __alignof__ (s47.m);
  printf ("max field align=%d\n", max_align);
  return 0;
}
