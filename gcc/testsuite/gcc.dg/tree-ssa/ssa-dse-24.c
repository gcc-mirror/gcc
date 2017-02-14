/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */


typedef unsigned int wchar_t;
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
} info;

void bar (struct printf_info *);

void foo(int prec,
  int width,
  wchar_t spec,
  unsigned int is_long_double,
  unsigned int is_short,
  unsigned int is_long,
  unsigned int alt,
  unsigned int space,
  unsigned int left,
  unsigned int showsign,
  unsigned int group,
  wchar_t pad)
{
    struct printf_info info = {
        .prec = prec,
        .width = width,
        .spec = spec,
        .is_long_double = is_long_double,
        .is_short = is_short,
        .is_long = is_long,
        .alt = alt,
        .space = space,
        .left = left,
        .showsign = showsign,
        .group = group,
        .pad = pad,
        .extra = 0,
        .wide = sizeof (char) != 1 };

    bar (&info);
}

/* { dg-final { scan-tree-dump-times "MEM\\\[\\(struct printf_info \\*\\)&info \\+ \[0-9\]+B\\\] = {}" 1 "dse1" } } */
