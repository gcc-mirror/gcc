/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-march=armv7-a -mfloat-abi=hard -mfpu=neon -O2" } */
/* { dg-skip-if "need SIMD instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-skip-if "need SIMD instructions" { *-*-* } { "-mfpu=vfp*" } { "" } } */

#define BUF 100
long a[BUF];

typedef unsigned int size_t;
typedef unsigned int wchar_t;
void *memset (void *s, int c, size_t n);
struct printf_info
{
  int prec;
  int width;
  wchar_t spec;
  unsigned int is_long_double:1;
  unsigned int is_short:1;
  unsigned int is_long:1;
  unsigned int alt:1;
  unsigned int space:1;
  unsigned int left:1;
  unsigned int showsign:1;
  unsigned int group:1;
  unsigned int extra:1;
  unsigned int is_char:1;
  unsigned int wide:1;
  unsigned int i18n:1;
  unsigned int __pad:4;
  unsigned short int user;
  wchar_t pad;
};

void bar (int *alt, int *space, int *left, int *showsign,
	  int *group,
	  int *is_long_double,
	  int *is_short,
	  int *is_long,
	  int *width,
	  int *prec,
	  int *use_outdigits,
	  unsigned int *pad,
	  wchar_t *spec);
void __printf_fp (char *s, struct printf_info *pinfo);
int foo(char *s)
{
  int alt = 0;
  int space = 0;
  int left = 0;
  int showsign = 0;
  int group = 0;
  int is_long_double = 0;
  int is_short = 0;
  int is_long = 0;
  int width = 0;
  int prec = -1;
  int use_outdigits = 0;
  unsigned int pad = L' ';
  wchar_t spec;

  bar (&alt, &space, &left, &showsign, &group, &is_long_double,
       &is_short, &is_long, &width, &prec, &use_outdigits, &pad, &spec);

  a[1] = a[0] + a[2] + a[3] + a[4] + a[5] + a[6];
  a[2] = a[1] + a[3] + a[5] + a[5] + a[6] + a[7];
  a[3] = a[2] + a[5] + a[7] + a[6] + a[7] + a[8];
  a[4] = a[3] + a[7] + a[11] + a[7] + a[8] + a[9];
  a[5] = a[5] + a[11] + a[13] + a[8] + a[9] + a[10];
  a[6] = a[7] + a[13] + a[17] + a[9] + a[10] + a[11];
  a[7] = a[11] + a[17] + a[19] + a[10] + a[11] + a[12];
  a[8] = a[17] + a[19] + a[23] + a[29] + a[31] + a[37];

  {
    struct printf_info info;
    memset (&info, 0, sizeof (struct printf_info));
    info.prec = prec;
    info.width = width;
    info.spec = spec;
    info.is_long_double = is_long_double;
    info.is_short = is_short;
    info.is_long = is_long;
    info.alt = alt;
    info.space = space;
    info.left = left;
    info.showsign = showsign;
    info.group = group;
    info.pad = pad;
    info.extra = 0;
    info.i18n = use_outdigits;
    info.wide = sizeof (wchar_t) != 1;

    __printf_fp (s, &info);
  }

  return 0;
}

