/* PR target/113609 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "\[ \\t\]+sete" 4 } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+setne" 4 } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+je" 4 } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+jne" 4 } } */

#include <immintrin.h>

unsigned int
cmp_pi8_setcc(char a)
{
    if (a == -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi16_setcc(short a)
{
    if (a == -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi32_setcc(int a)
{
    if (a == -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi64_setcc(long long a)
{
    if (a == -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi8_setne(char a)
{
    if (a != -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi16_setne(short a)
{
    if (a != -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi32_setne(int a)
{
    if (a != -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_pi64_setne(long long a)
{
    if (a != -1)
      return 1;
    else
      return 0;
}

__m128i
cmp_pi8_je(__m128i a, char b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m128i
cmp_pi16_je(__m128i a, short b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m128i
cmp_pi32_je(__m128i a, int b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m128i
cmp_pi64_je(__m128i a, long long b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m128i
cmp_pi8_jne(__m128i a, char b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m128i
cmp_pi16_jne(__m128i a, short b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m128i
cmp_pi32_jne(__m128i a, int b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m128i
cmp_pi64_jne(__m128i a, long long b) {
    if (b == -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}
