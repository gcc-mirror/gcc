double f1 (int, long, float);
double f2 (int, long, float);
#pragma omp declare variant (f1) match (user={condition(0)},construct={parallel})
double f3 (int, long, float);
#pragma omp declare variant (f1) match (construct={parallel},user={condition(score(1):1)})
double f4 (int, long, float);
double f5 (int, long, float);
#pragma omp declare variant (f5) match (user={condition(0)})
double f6 (int, long, float);
#pragma omp declare variant (f5) match (construct={parallel},user={condition(score(1):1)})	/* { dg-error "'f5' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f7 (int, long, float);
double f8 (int, long, float);
#pragma omp declare variant (f8) match (user={condition(0)},construct={for})
double f9 (int, long, float);
#pragma omp declare variant (f8) match (user={condition(1)})					/* { dg-error "'f8' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f10 (int, long, float);
double f11 (int, long, float);
#pragma omp declare variant (f11) match (construct={target,teams,parallel,for})
double f12 (int, long, float);
#pragma omp declare variant (f11) match (user={condition(score(1):1)},construct={target,teams,parallel,for})
double f13 (int, long, float);
#pragma omp declare variant (f11) match (implementation={vendor(gnu)},construct={target,teams,parallel})	/* { dg-error "'f11' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f14 (int, long, float);
#pragma omp declare variant (f11) match (device={kind(any)},construct={teams,parallel})		/* { dg-error "'f11' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f15 (int, long, float);
double f16 (int, long, float);
#pragma omp declare variant (f16) match (construct={teams,parallel})
double f17 (int, long, float);
#pragma omp declare variant (f16) match(construct={teams,parallel,for})				/* { dg-error "'f16' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f18 (int, long, float);
double f19 (int, long, float);
#pragma omp declare variant (f19) match (construct={parallel})
double f20 (int, long, float);
#pragma omp declare variant (f19) match (construct={for},implementation={vendor(gnu,llvm)})	/* { dg-error "'f19' used as a variant with incompatible 'constructor' selector sets" "" { target c } } */
double f21 (int, long, float);
