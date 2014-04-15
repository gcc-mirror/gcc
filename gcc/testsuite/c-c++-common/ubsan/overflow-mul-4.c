/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */


int
main ()
{
#define MUL_WITH_CHECK(xv, yv, zv) \
  do {						\
    long long x = xv;				\
    long long y = yv;				\
    long long z;				\
    asm ("" : "+g" (x));			\
    asm ("" : "+g" (y));			\
    z = x * y;					\
    asm ("" : "+g" (z));			\
    if (z != zv)				\
      __builtin_abort ();			\
  } while (0)
  MUL_WITH_CHECK (0x1555555555555556LL, 6LL, -0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (-0x1555555555555556LL, -6LL, -0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (0x1555555555555556LL, -6LL, 0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (-0x1555555555555556LL, 6LL, 0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (0x81234568LL, 0xfdbe971fLL, -0x7fffffff439a4068LL);
  MUL_WITH_CHECK (-0x81234568LL, -0xfdbe971fLL, -0x7fffffff439a4068LL);
  MUL_WITH_CHECK (0x81234568LL, -0xfdbe971fLL, 0x7fffffff439a4068LL);
  MUL_WITH_CHECK (-0x81234568LL, 0xfdbe971fLL, 0x7fffffff439a4068LL);
  MUL_WITH_CHECK (0x1555555555555555LL, 7LL, -0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (-0x1555555555555555LL, -7LL, -0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (0x1555555555555555LL, -7LL, 0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (-0x1555555555555555LL, 7LL, 0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (0x81234567LL, 0xfdbe9720LL, -0x7fffffffc0359220LL);
  MUL_WITH_CHECK (-0x81234567LL, -0xfdbe9720LL, -0x7fffffffc0359220LL);
  MUL_WITH_CHECK (0x81234567LL, -0xfdbe9720LL, 0x7fffffffc0359220LL);
  MUL_WITH_CHECK (-0x81234567LL, 0xfdbe9720LL, 0x7fffffffc0359220LL);
  MUL_WITH_CHECK (6LL, 0x1555555555555556LL, -0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (-6LL, -0x1555555555555556LL, -0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (-6LL, 0x1555555555555556LL, 0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (6LL, -0x1555555555555556LL, 0x7ffffffffffffffcLL);
  MUL_WITH_CHECK (0xfdbe971fLL, 0x81234568LL, -0x7fffffff439a4068LL);
  MUL_WITH_CHECK (-0xfdbe971fLL, -0x81234568LL, -0x7fffffff439a4068LL);
  MUL_WITH_CHECK (-0xfdbe971fLL, 0x81234568LL, 0x7fffffff439a4068LL);
  MUL_WITH_CHECK (0xfdbe971fLL, -0x81234568LL, 0x7fffffff439a4068LL);
  MUL_WITH_CHECK (7LL, 0x1555555555555555LL, -0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (-7LL, -0x1555555555555555LL, -0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (-7LL, 0x1555555555555555LL, 0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (7LL, -0x1555555555555555LL, 0x6aaaaaaaaaaaaaadLL);
  MUL_WITH_CHECK (0xfdbe9720LL, 0x81234567LL, -0x7fffffffc0359220LL);
  MUL_WITH_CHECK (-0xfdbe9720LL, -0x81234567LL, -0x7fffffffc0359220LL);
  MUL_WITH_CHECK (-0xfdbe9720LL, 0x81234567LL, 0x7fffffffc0359220LL);
  MUL_WITH_CHECK (0xfdbe9720LL, -0x81234567LL, 0x7fffffffc0359220LL);
  return 0;
}

/* { dg-output "overflow-mul-4.c:20:\[^\n\r]*signed integer overflow: 1537228672809129302 \\* 6 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:21:\[^\n\r]*signed integer overflow: -1537228672809129302 \\* -6 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:22:\[^\n\r]*signed integer overflow: 1537228672809129302 \\* -6 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:23:\[^\n\r]*signed integer overflow: -1537228672809129302 \\* 6 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:24:\[^\n\r]*signed integer overflow: 2166572392 \\* 4257126175 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:25:\[^\n\r]*signed integer overflow: -2166572392 \\* -4257126175 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:26:\[^\n\r]*signed integer overflow: 2166572392 \\* -4257126175 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:27:\[^\n\r]*signed integer overflow: -2166572392 \\* 4257126175 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:28:\[^\n\r]*signed integer overflow: 1537228672809129301 \\* 7 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:29:\[^\n\r]*signed integer overflow: -1537228672809129301 \\* -7 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:30:\[^\n\r]*signed integer overflow: 1537228672809129301 \\* -7 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:31:\[^\n\r]*signed integer overflow: -1537228672809129301 \\* 7 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:32:\[^\n\r]*signed integer overflow: 2166572391 \\* 4257126176 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:33:\[^\n\r]*signed integer overflow: -2166572391 \\* -4257126176 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:34:\[^\n\r]*signed integer overflow: 2166572391 \\* -4257126176 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:35:\[^\n\r]*signed integer overflow: -2166572391 \\* 4257126176 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:36:\[^\n\r]*signed integer overflow: 6 \\* 1537228672809129302 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:37:\[^\n\r]*signed integer overflow: -6 \\* -1537228672809129302 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:38:\[^\n\r]*signed integer overflow: -6 \\* 1537228672809129302 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:39:\[^\n\r]*signed integer overflow: 6 \\* -1537228672809129302 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:40:\[^\n\r]*signed integer overflow: 4257126175 \\* 2166572392 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:41:\[^\n\r]*signed integer overflow: -4257126175 \\* -2166572392 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:42:\[^\n\r]*signed integer overflow: -4257126175 \\* 2166572392 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:43:\[^\n\r]*signed integer overflow: 4257126175 \\* -2166572392 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:44:\[^\n\r]*signed integer overflow: 7 \\* 1537228672809129301 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:45:\[^\n\r]*signed integer overflow: -7 \\* -1537228672809129301 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:46:\[^\n\r]*signed integer overflow: -7 \\* 1537228672809129301 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:47:\[^\n\r]*signed integer overflow: 7 \\* -1537228672809129301 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:48:\[^\n\r]*signed integer overflow: 4257126176 \\* 2166572391 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:49:\[^\n\r]*signed integer overflow: -4257126176 \\* -2166572391 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:50:\[^\n\r]*signed integer overflow: -4257126176 \\* 2166572391 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*overflow-mul-4.c:51:\[^\n\r]*signed integer overflow: 4257126176 \\* -2166572391 cannot be represented in type 'long long int'\[^\n\r]*" } */
