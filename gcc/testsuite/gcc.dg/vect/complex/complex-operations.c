#include <stdio.h>
#include <complex.h>

#ifndef PREF
#define PREF c
#endif

#define FX(N,P) P ## _ ## N
#define MK(N,P) FX(P,N)

#define N 32
#define TYPE double

// ------ FMA

// Complex FMA instructions rotating the result

__attribute__((noinline,noipa))
void MK(fma0, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(fma90, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * b[i] * I;
}

__attribute__((noinline,noipa))
void MK(fma180, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * b[i] * I * I;
}

__attribute__((noinline,noipa))
void MK(fma270, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * b[i] * I * I * I;
}

// Complex FMA instructions rotating the second parameter.


__attribute__((noinline,noipa))
void MK(fma0_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(fma90_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * (b[i] * I);
}

__attribute__((noinline,noipa))
void MK(fma180_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * (b[i] * I * I);
}

__attribute__((noinline,noipa))
void MK(fma270_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * (b[i] * I * I * I);
}

// Complex FMA instructions with conjucated values.


__attribute__((noinline,noipa))
void MK(fma_conj_first, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += conj (a[i]) * b[i];
}

__attribute__((noinline,noipa))
void MK(fma_conj_second, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += a[i] * conj (b[i]);
}

__attribute__((noinline,noipa))
void MK(fma_conj_both, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] += conj (a[i]) * conj (b[i]);
}

// ----- FMS

// Complex FMS instructions rotating the result

__attribute__((noinline,noipa))
void MK(fms0, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(fms90, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * b[i] * I;
}

__attribute__((noinline,noipa))
void MK(fms180, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * b[i] * I * I;
}

__attribute__((noinline,noipa))
void MK(fms270, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * b[i] * I * I * I;
}

// Complex FMS instructions rotating the second parameter.

__attribute__((noinline,noipa))
void MK(fms0_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(fms90_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * (b[i] * I);
}

__attribute__((noinline,noipa))
void MK(fms180_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * (b[i] * I * I);
}

__attribute__((noinline,noipa))
void MK(fms270_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * (b[i] * I * I * I);
}

// Complex FMS instructions with conjucated values.

__attribute__((noinline,noipa))
void MK(fms_conj_first, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= conj (a[i]) * b[i];
}

__attribute__((noinline,noipa))
void MK(fms_conj_second, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= a[i] * conj (b[i]);
}

__attribute__((noinline,noipa))
void MK(fms_conj_both, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] -= conj (a[i]) * conj (b[i]);
}


// ----- MUL

// Complex MUL instructions rotating the result

__attribute__((noinline,noipa))
void MK(mul0, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(mul90, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * b[i] * I;
}

__attribute__((noinline,noipa))
void MK(mul180, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * b[i] * I * I;
}

__attribute__((noinline,noipa))
void MK(mul270, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * b[i] * I * I * I;
}

// Complex MUL instructions rotating the second parameter.

__attribute__((noinline,noipa))
void MK(mul0_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * b[i];
}

__attribute__((noinline,noipa))
void MK(mul90_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * (b[i] * I);
}

__attribute__((noinline,noipa))
void MK(mul180_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * (b[i] * I * I);
}

__attribute__((noinline,noipa))
void MK(mul270_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * (b[i] * I * I * I);
}

// Complex FMS instructions with conjucated values.

__attribute__((noinline,noipa))
void MK(mul_conj_first, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = conj (a[i]) * b[i];
}

__attribute__((noinline,noipa))
void MK(mul_conj_second, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] * conj (b[i]);
}

__attribute__((noinline,noipa))
void MK(mul_conj_both, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = conj (a[i]) * conj (b[i]);
}


// ----- ADD

// Complex ADD instructions rotating the result

__attribute__((noinline,noipa))
void MK(add0, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + b[i];
}

__attribute__((noinline,noipa))
void MK(add90, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = (a[i] + b[i]) * I;
}

__attribute__((noinline,noipa))
void MK(add180, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = (a[i] + b[i]) * I * I;
}

__attribute__((noinline,noipa))
void MK(add270, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = (a[i] + b[i]) * I * I * I;
}

// Complex ADD instructions rotating the second parameter.

__attribute__((noinline,noipa))
void MK(add0_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + b[i];
}

__attribute__((noinline,noipa))
void MK(add90_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + (b[i] * I);
}

__attribute__((noinline,noipa))
void MK(add180_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + (b[i] * I * I);
}

__attribute__((noinline,noipa))
void MK(add270_snd, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + (b[i] * I * I * I);
}

// Complex ADD instructions with conjucated values.

__attribute__((noinline,noipa))
void MK(add_conj_first, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = conj (a[i]) + b[i];
}

__attribute__((noinline,noipa))
void MK(add_conj_second, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = a[i] + conj (b[i]);
}

__attribute__((noinline,noipa))
void MK(add_conj_both, PREF) (TYPE complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
      c[i] = conj (a[i]) + conj (b[i]);
}


