void f(int*,int *,int*);
void f0(int*,int *,int*);
void f1(int*,int *,int*);
void f2(int*,int *,int*);
void f3(int*,int *,int*);
void f4(int*,int *,int*);
void f5(int*,int *,int*);
void f6(int*,int *,int*);
void f7(int*,int *,int*);
void f8(int*,int *,int*);
void f9(int*,int *,int*);
void fa(int*,int *,int*);
void f10(int*,int *,int*);
void f11(int*,int *,int*);
void f12(int*,int *,int*);
void f13(int*,int *,int*);
void f14(int*,int *,int*);
void f15(int*,int *,int*);
void f16(int*,int *,int*);

#pragma omp declare variant(f) match(construct={dispatch}) adjust_args(x : y) 			// { dg-error "expected 'nothing' or 'need_device_ptr'" }
#pragma omp declare variant(f0) match(construct={dispatch}) adjust_args(x) 			// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f1) match(construct={dispatch}) adjust_args(x,) 			// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f2) match(construct={dispatch}) adjust_args(foo x) 			// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f3) match(construct={dispatch}) adjust_args(nothing) 		// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f4) match(construct={dispatch}) adjust_args(need_device_ptr)	// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f5) match(construct={dispatch}) adjust_args(nothing x)	 	// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f6) match(construct={dispatch}) adjust_args(need_device_ptr x)	// { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" }
#pragma omp declare variant(f7) match(construct={dispatch}) adjust_args(need_device_addr x) 	// { dg-error "expected 'nothing' or 'need_device_ptr'" }
#pragma omp declare variant(f8) match(construct={dispatch}) adjust_args(nothing :)	 	// { dg-error "expected expression before '\\)' token" }
#pragma omp declare variant(f9) match(construct={dispatch}) adjust_args(need_device_ptr :) 	// { dg-error "expected expression before '\\)' token" }
#pragma omp declare variant(fa) match(construct={dispatch}) adjust_args(need_device_addr :) 	// { dg-error "expected 'nothing' or 'need_device_ptr'" }
// { dg-note "73: 'need_device_addr' is not valid for C" "" { target *-*-* } .-1 }
#pragma omp declare variant(f10) match(construct={dispatch}) adjust_args(need_device_ptr : omp_num_args-1) 	// { dg-error "expected ':' before '\\)' token" }
// { dg-note "92: an expression is only allowed in a numeric range" "" { target *-*-* } .-1 }

// Valid:
#pragma omp declare variant(f11) match(construct={dispatch}) adjust_args(nothing : z, 1:2)
#pragma omp declare variant(f12) match(construct={dispatch}) adjust_args(need_device_ptr : x)
#pragma omp declare variant(f13) match(construct={dispatch}) adjust_args(need_device_addr : y)	// { dg-error "expected 'nothing' or 'need_device_ptr'" }
// { dg-note "74: 'need_device_addr' is not valid for C" "" { target *-*-* } .-1 }
#pragma omp declare variant(f14) match(construct={dispatch}) adjust_args(nothing : :)
#pragma omp declare variant(f15) match(construct={dispatch}) adjust_args(need_device_ptr : 3:3)
#pragma omp declare variant(f16) match(construct={dispatch}) adjust_args(need_device_addr : 2:2)// { dg-error "expected 'nothing' or 'need_device_ptr'" }
// { dg-note "74: 'need_device_addr' is not valid for C" "" { target *-*-* } .-1 }

void g(int*x, int *y, int *z);
