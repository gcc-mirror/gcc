/* { dg-do compile */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stdlib.h>
#include <stdio.h>
#include <altivec.h>

vector double retd;
vector float retf;
vector signed int retsi;

void test_int(vector signed int a, const int b)
{
	retf = vec_ctf(a,b); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-1); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-31); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,1);
	retf = vec_ctf(a,31);
	retf = vec_ctf(a,32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,42); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
}

void test_uint(vector unsigned int a, const int b)
{
	retf = vec_ctf(a,b); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-1); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-31); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,-32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,1);
	retf = vec_ctf(a,31);
	retf = vec_ctf(a,32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retf = vec_ctf(a,42); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
}

void test_longlong(vector signed long long a, const int b,int x)
{
	retd = vec_ctf(a,b); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive"  } */
	retd = vec_ctf(a,-1); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,-31); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,-32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,1);
	retd = vec_ctf(a,31);
	retd = vec_ctf(a,32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,42); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
}

void test_ulonglong(vector unsigned long long a, const int b,int x)
{
	retd = vec_ctf(a,b); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,-1); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,-31); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,-32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,1);
	retd = vec_ctf(a,31);
	retd = vec_ctf(a,32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retd = vec_ctf(a,42); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
}


void test_cts_1(vector float a, const int b)
{
	retsi = vec_cts(a,b); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retsi = vec_cts(a,-1); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retsi = vec_cts(a,-31); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retsi = vec_cts(a,-32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retsi = vec_cts(a,1);
	retsi = vec_cts(a,31);
	retsi = vec_cts(a,32); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
	retsi = vec_cts(a,42); /* { dg-error "argument 2 must be a literal between 0 and 31, inclusive" } */
}

