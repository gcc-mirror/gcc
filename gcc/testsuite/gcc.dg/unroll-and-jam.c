/* { dg-do run } */
/* { dg-options "-O3 -floop-unroll-and-jam --param unroll-jam-min-percent=0 -fdump-tree-unrolljam-details" } */
/* { dg-require-effective-target int32plus } */

#include <stdio.h>
extern unsigned int a[];
extern unsigned int b[];
extern unsigned int aa[][1024];
unsigned int checksum;
void checkaa(void)
{
  unsigned sum = 1;
  unsigned long i, j;
  for (i = 0; i < 1024; i++) {
      for (j = 0; j < 16; j++) {
	  sum += aa[j][i]*31+47;
      }
  }
  checksum = checksum * 27 + sum;
  //printf("  %d\n", sum);
}

void checkb(void)
{
  unsigned sum = 1;
  unsigned long i, j;
  for (i = 0; i < 1024; i++) {
      sum += b[i]*31+47;
  }
  checksum = checksum * 27 + sum;
  //printf("  %d\n", sum);
}

#define TEST(name, body, test) \
static void __attribute__((noinline,noclone)) name (unsigned long n, unsigned long m) \
{ \
  unsigned long i, j; \
  for (i = 1; i < m; i++) { \
      for (j = 1; j < n; j++) { \
	  body; \
      } \
  } \
  test; \
} \
static void __attribute__((noinline,noclone,optimize("O1"))) name ## noopt (unsigned long n, unsigned long m) \
{ \
  unsigned long i, j; \
  for (i = 1; i < m; i++) { \
      for (j = 1; j < n; j++) { \
	  body; \
      } \
  } \
  test; \
}
TEST(foo1, aa[i+1][j+1]=aa[i][j] * aa[i][j] / 2, checkaa()) //ok, -1,-1
TEST(foo2, aa[i][j+1]=3*aa[i+1][j], checkaa()) //notok, 1,-1
TEST(foo3, aa[i+1][j-1]=aa[i][j] * aa[i][j] / 2, checkaa()) //notok, -1,1
TEST(foo4, aa[i][j] = aa[i-1][j+1] * aa[i-1][j+1] / 2, checkaa()) //notok, -1,1
TEST(foo5, aa[i][j] = aa[i+1][j+1] * aa[i+1][j+1] / 2, checkaa()) //ok, 1,1
TEST(foo6, aa[i][j] = aa[i+1][j] * aa[i+1][j] / 2, checkaa()) //ok, -1,0
TEST(foo7, aa[i+1][j] = aa[i][j] * aa[i][j] / 2, checkaa()) //ok, 1,0
TEST(foo9, b[j] = 3*b[j+1] + 1, checkb()) //notok, 0,-1
TEST(foo10, b[j] = 3*b[j] + 1, checkb()) //ok, 0,0

/* foo8 should work as well, but currently doesn't because the distance
   vectors we compute are too pessimistic.  We compute
     (0,1), (1,1) and (1,-1) 
   and the last one causes us to lose.  */
TEST(foo8, b[j+1] = 3*b[j] + 1, checkb()) //ok, 0,1

unsigned int a[1024];
unsigned int b[1024];
unsigned int aa[16][1024];
void init(void)
{
  unsigned long i,j;
  for (i = 0; i < 1024; i++) {
      for (j = 0; j < 16; j++) {
	  aa[j][i] = ((j+1)*2+i+1) % 17;
      }
      a[i] = ((i+1)*31) % 19;
      b[i] = ((i+1)*47) % 23;
  }
  checksum = 1;
}

#define RUN(name) \
    printf(" %s\n", #name); \
    init();for(i=0;i<4;i++)name##noopt(32,8); checka = checksum; \
    init();for(i=0;i<4;i++)name(32,8); \
    printf("%sok %s\n", checka != checksum ? "NOT " : "", #name);

int main()
{
  int i;
  unsigned checka;
  RUN(foo1);
  RUN(foo2);
  RUN(foo3);
  RUN(foo4);
  RUN(foo5);
  RUN(foo6);
  RUN(foo7);
  RUN(foo8);
  RUN(foo9);
  RUN(foo10);
  return 0;
}

/* Five loops should be unroll-jammed (actually six, but see above).  */
/* { dg-final { scan-tree-dump-times "applying unroll and jam" 5 "unrolljam" } } */
