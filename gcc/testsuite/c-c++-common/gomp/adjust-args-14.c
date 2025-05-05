/* Multiple uses of the same name should not be diagnosed multiple times.  */

/* No parameters.  */

void v00();
#pragma omp declare variant(v00) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: w, /* { dg-error "'w' is not a function parameter" } */ \
							      w) /* { dg-bogus "'w' is not a function parameter" "" { xfail *-*-* } } */
void b00();

void v01() {}
#pragma omp declare variant(v01) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: ww, /* { dg-error "'ww' is not a function parameter" } */ \
							      ww) /* { dg-bogus "'ww' is not a function parameter" "" { xfail *-*-* } } */
void b01() {}

/* No parameters, specified with void.  */

void v10(void);
#pragma omp declare variant(v10) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: x, /* { dg-error "'x' is not a function parameter" } */ \
							      x) /* { dg-bogus "'x' is not a function parameter" "" { xfail *-*-* } } */
void b10(void);

void v11(void) {}
#pragma omp declare variant(v11) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: xx, /* { dg-error "'xx' is not a function parameter" } */ \
							      xx) /* { dg-bogus "'xx' is not a function parameter" "" { xfail *-*-* } } */
void b11(void) {}

/* Variadic.  */

void v20(...);
#pragma omp declare variant(v20) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: y, /* { dg-error "'y' is not a function parameter" } */ \
							      y) /* { dg-bogus "'y' is not a function parameter" "" { xfail *-*-* } } */
void b20(...);

void v21(...) {}
#pragma omp declare variant(v21) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: yy, /* { dg-error "'yy' is not a function parameter" } */ \
							      yy) /* { dg-bogus "'yy' is not a function parameter" "" { xfail *-*-* } } */
void b21(...) {}

/* With non-empty parameter list.  */

void v30(int a);
#pragma omp declare variant(v30) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: z, /* { dg-error "'z' is not a function parameter" } */ \
							      z) /* { dg-bogus "'z' is not a function parameter" "" { xfail *-*-* } } */
void b30(int a);

void v31(int a) { (void)a; }
#pragma omp declare variant(v31) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: zz, /* { dg-error "'zz' is not a function parameter" } */ \
							      zz) /* { dg-bogus "'zz' is not a function parameter" "" { xfail *-*-* } } */
void b31(int a) { (void)a; }