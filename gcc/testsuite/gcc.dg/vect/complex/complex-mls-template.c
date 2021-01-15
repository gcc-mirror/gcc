#include <complex.h>

void fms0 (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	   _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * b[i];
}

void fms90snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	       _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * (b[i] * I);
}

void fms180snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * (b[i] * I * I);
}

void fms270snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * (b[i] * I * I * I);
}

void fms90fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	       _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= (a[i] * I) * b[i];
}

void fms180fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= (a[i] * I * I) * b[i];
}

void fms270fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= (a[i] * I * I * I) * b[i];
}

void fmsconjfst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		 _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= ~a[i] * b[i];
}

void fmsconjsnd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		 _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * ~b[i];
}

void fmsconjboth (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		  _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= ~a[i] * ~b[i];
}

void fms_elem (_Complex TYPE a[restrict N], _Complex TYPE b,
	       _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * b;
}


void fms_elemconjfst (_Complex TYPE a[restrict N], _Complex TYPE b,
		      _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= ~a[i] * b;
}

void fms_elemconjsnd (_Complex TYPE a[restrict N], _Complex TYPE b,
		      _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= a[i] * ~b;
}

void fms_elemconjboth (_Complex TYPE a[restrict N], _Complex TYPE b,
		       _Complex TYPE c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] -= ~a[i] * ~b;
}

