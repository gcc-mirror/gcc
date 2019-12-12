/* { dg-do compile } */
/* { dg-additional-options "-Wno-volatile" { target c++ } } */

int x;
volatile int y;
volatile unsigned char z;

void f1(void)
{
  #pragma omp atomic
    x++;
  #pragma omp atomic
    x--;
  #pragma omp atomic
    ++x;
  #pragma omp atomic
    --x;
  #pragma omp atomic
    x += 1;
  #pragma omp atomic
    x -= y;
  #pragma omp atomic
    x |= 1;
  #pragma omp atomic
    x &= 1;
  #pragma omp atomic
    x ^= 1;
  #pragma omp atomic
    x *= 3;
  #pragma omp atomic
    x /= 3;
  #pragma omp atomic
    x /= 3;
  #pragma omp atomic
    x <<= 3;
  #pragma omp atomic
    x >>= 3;
}

void f2(void)
{
  #pragma omp atomic
    y++;
  #pragma omp atomic
    y--;
  #pragma omp atomic
    ++y;
  #pragma omp atomic
    --y;
  #pragma omp atomic
    y += 1;
  #pragma omp atomic
    y -= x;
  #pragma omp atomic
    y |= 1;
  #pragma omp atomic
    y &= 1;
  #pragma omp atomic
    y ^= 1;
  #pragma omp atomic
    y *= 3;
  #pragma omp atomic
    y /= 3;
  #pragma omp atomic
    y /= 3;
  #pragma omp atomic
    y <<= 3;
  #pragma omp atomic
    y >>= 3;
}

void f3(void)
{
  #pragma omp atomic
    z++;
  #pragma omp atomic
    z--;
  #pragma omp atomic
    ++z;
  #pragma omp atomic
    --z;
  #pragma omp atomic
    z += 1;
  #pragma omp atomic
    z |= 1;
  #pragma omp atomic
    z &= 1;
  #pragma omp atomic
    z ^= 1;
  #pragma omp atomic
    z *= 3;
  #pragma omp atomic
    z /= 3;
  #pragma omp atomic
    z /= 3;
  #pragma omp atomic
    z <<= 3;
  #pragma omp atomic
    z >>= 3;
}
