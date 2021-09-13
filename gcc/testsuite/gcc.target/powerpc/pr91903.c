/* { dg-do compile */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <stdlib.h>
#include <stdio.h>
#include <altivec.h>

vector double retd;
vector float retf;
vector signed int retsi;

void test_int(vector signed int a, const int b)
{
	retf = vec_ctf(a,b); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-1); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-31); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,1);
	retf = vec_ctf(a,31);
	retf = vec_ctf(a,32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,42); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
}

void test_uint(vector unsigned int a, const int b)
{
	retf = vec_ctf(a,b); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-1); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-31); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,-32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,1);
	retf = vec_ctf(a,31);
	retf = vec_ctf(a,32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retf = vec_ctf(a,42); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
}

void test_longlong(vector signed long long a, const int b,int x)
{
	retd = vec_ctf(a,b); /* { dg-error "argument 2 must be a 5-bit unsigned literal"  } */
	retd = vec_ctf(a,-1); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,-31); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,-32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,1);
	retd = vec_ctf(a,31);
	retd = vec_ctf(a,32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,42); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
}

void test_ulonglong(vector unsigned long long a, const int b,int x)
{
	retd = vec_ctf(a,b); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,-1); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,-31); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,-32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,1);
	retd = vec_ctf(a,31);
	retd = vec_ctf(a,32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retd = vec_ctf(a,42); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
}


void test_cts_1(vector float a, const int b)
{
	retsi = vec_cts(a,b); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retsi = vec_cts(a,-1); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retsi = vec_cts(a,-31); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retsi = vec_cts(a,-32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retsi = vec_cts(a,1);
	retsi = vec_cts(a,31);
	retsi = vec_cts(a,32); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
	retsi = vec_cts(a,42); /* { dg-error "argument 2 must be a 5-bit unsigned literal" } */
}

