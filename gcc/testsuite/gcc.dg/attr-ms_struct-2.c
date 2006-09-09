/* Test for MS structure sizes.  */
/* { dg-do run { target *-*-interix* *-*-mingw* *-*-cygwin* i?86-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-std=gnu99" } */

extern void abort ();

#define ATTR __attribute__((__ms_struct__))

struct _struct_0
{
  long  member_0   : 25 ;
  short  member_1   : 6 ;
  char  member_2   : 2 ;
  unsigned  short  member_3   : 1 ;
  unsigned  char  member_4   : 7 ;
  short  member_5   : 16 ;
  long  : 0 ;
  char  member_7  ;

} ATTR;
typedef struct _struct_0 struct_0;

#define size_struct_0 20

struct_0 test_struct_0 = { 18557917, 17, 3, 0, 80, 6487, 93 };

int
main (void)
{

  if (size_struct_0 != sizeof (struct_0))
    abort ();

  return 0;
}
