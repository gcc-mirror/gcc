/* bf-no-ms-layout.c */

/* Test for gcc bitfield layout, with -mno-ms-bitfields */
/* Adapted from Donn Terry <donnte@microsoft.com> testcase
   posted to GCC-patches
   http://gcc.gnu.org/ml/gcc-patches/2000-08/msg00577.html */

/* { dg-do run { target *-*-mingw* *-*-cygwin* i?86-*-darwin } } */
/* { dg-options "-mno-ms-bitfields" } */

#include <stddef.h>
#include <string.h>

extern void abort();

#pragma pack(8)

struct one {
  int d;
  unsigned char a;
  unsigned short b:7;
  char c;
};

struct two {
  int d;
  unsigned char a;
  unsigned int b:7;
  char c;
};

struct three {
  short d;
  unsigned short a:3;
  unsigned short b:9;
  unsigned char c:7;
};


/* Bitfields of size 0 have some truly odd behaviors. */

struct four {
  unsigned short a:3;
  unsigned short b:9;
  unsigned int :0;  /* forces struct alignment to int */
  unsigned char c:7;
};

struct five {
  char a;
  int :0;        /* ignored; prior field is not a bitfield. */
  char b;
  char c;
};

struct six {
  char a :8;
  int :0;	/* not ignored; prior field IS a bitfield, causes
		   struct alignment as well. */
  char b;
  char c;
} ;

struct seven {
  char a:8;
  char :0;
  int  :0;	/* Ignored; prior field is zero size bitfield. */
  char b;
  char c;
};

struct eight { /* ms size 4 */
  short b:3;
  char  c;
};

#ifdef _MSC_VER
#define LONGLONG __int64
#else
#define LONGLONG long long
#endif

union nine {   /* ms size 8 */
  LONGLONG a:3;
  char  c;
};

struct ten {   /* ms size 16 */
  LONGLONG a:3;
  LONGLONG b:3;
  char  c;
};


#define val(s,f) (s.f)

#define check_struct(_X) \
{ \
  if (sizeof (struct _X) != exp_sizeof_##_X )	\
    abort();					\
  memcpy(&test_##_X, filler, sizeof(test_##_X));\
  if (val(test_##_X,c) != exp_##_X##_c) 	\
     abort();					\
}

#define check_union(_X) \
{ \
  if (sizeof (union _X) != exp_sizeof_##_X )	\
    abort();                                    \
  memcpy(&test_##_X, filler, sizeof(test_##_X));\
  if (val(test_##_X,c) != exp_##_X##_c) 	\
     abort();					\
}

#define check_struct_size(_X) \
{ \
  if (sizeof (struct _X) != exp_sizeof_##_X )	\
    abort();                                    \
}

#define check_struct_off(_X) \
{ \
  memcpy(&test_##_X, filler, sizeof(test_##_X));\
  if (val(test_##_X,c) != exp_##_X##_c) 	\
    abort();                                    \
}

#define check_union_size(_X) \
{ \
  if (sizeof (union _X) != exp_sizeof_##_X )	\
    abort();                                    \
}

#define check_union_off(_X) \
{ \
  memcpy(&test_##_X, filler, sizeof(test_##_X));\
  if (val(test_##_X,c) != exp_##_X##_c) 	\
    abort();                                    \
}

int main(){

  unsigned char filler[16];
  struct one test_one;
  struct two test_two;
  struct three test_three;
  struct four test_four;
  struct five test_five;
  struct six test_six;
  struct seven test_seven;
  struct eight test_eight;
  union nine test_nine;
  struct ten test_ten;

#if defined (_TEST_MS_LAYOUT) || defined (_MSC_VER)
  size_t exp_sizeof_one = 12;
  size_t exp_sizeof_two = 16;
  size_t exp_sizeof_three =6;
  size_t exp_sizeof_four = 8;
  size_t exp_sizeof_five = 3;
  size_t exp_sizeof_six = 8;
  size_t exp_sizeof_seven = 3;
  size_t exp_sizeof_eight = 4;
  size_t exp_sizeof_nine = 8;
  size_t exp_sizeof_ten = 16;

  unsigned char exp_one_c = 8;
  unsigned char exp_two_c  = 12;
  unsigned char exp_three_c = 4;
  unsigned char exp_four_c = 4;
  char exp_five_c = 2;
  char exp_six_c = 5;
  char exp_seven_c = 2;
  char exp_eight_c = 2;
  char exp_nine_c = 0;
  char exp_ten_c = 8;

#else /* testing -mno-ms-bitfields */

  size_t exp_sizeof_one = 8;
  size_t exp_sizeof_two = 8;
  size_t exp_sizeof_three = 6;
  size_t exp_sizeof_four = 6;
  size_t exp_sizeof_five = 6;
  size_t exp_sizeof_six = 6;
  size_t exp_sizeof_seven = 6;
  size_t exp_sizeof_eight = 2;
  size_t exp_sizeof_nine = 8;
  size_t exp_sizeof_ten = 8;

  unsigned short exp_one_c = 6;
  unsigned int exp_two_c  = 6;
  unsigned char exp_three_c = 64;
  unsigned char exp_four_c = 4;
  char exp_five_c = 5;
  char exp_six_c = 5;
  char exp_seven_c = 5;
  char exp_eight_c = 1;
  char exp_nine_c = 0;
  char exp_ten_c = 1;

#endif

  unsigned char i;
  for ( i = 0; i < 16; i++ )
    filler[i] = i;

  check_struct_off (one);
  check_struct_off (two);
  check_struct_off (three);
  check_struct_off (four);
  check_struct_off (five);
  check_struct_off (six);
  check_struct_off (seven);
  check_struct_off (eight);
  check_union_off (nine);
  check_struct_off (ten);

  check_struct_size (one);
  check_struct_size (two);
  check_struct_size (three);
  check_struct_size (four);
  check_struct_size (five);
  check_struct_size (six);
  check_struct_size (seven);
  check_struct_size (eight);
  check_union_size (nine);
  check_struct_size (ten);

  return 0;
};
