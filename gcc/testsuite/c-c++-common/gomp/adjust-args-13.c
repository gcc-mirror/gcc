/* Depending on how the variable is looked up, such as by using undeclared_variable
   to emit a diagnostic in the c front end, we might accidently prevent diagnostics
   later in the file.
   Note, the names specified in the adjust_args clause are important for this test.  */

/* No parameters.  */

void v00();
#pragma omp declare variant(v00) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: w) /* { dg-error "'w' is not a function parameter" } */
void b00();

void v01() {}
#pragma omp declare variant(v01) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: w) /* { dg-error "'w' is not a function parameter" } */
void b01() {}

/* No parameters, specified with void.  */

void v10(void);
#pragma omp declare variant(v10) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: x) /* { dg-error "'x' is not a function parameter" } */
void b10(void);

void v11(void) {}
#pragma omp declare variant(v11) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: x) /* { dg-error "'x' is not a function parameter" } */
void b11(void) {}

/* Variadic.  */

void v20(...);
#pragma omp declare variant(v20) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: y) /* { dg-error "'y' is not a function parameter" } */
void b20(...);

void v21(...) {}
#pragma omp declare variant(v21) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: y) /* { dg-error "'y' is not a function parameter" } */
void b21(...) {}

/* With non-empty parameter list.  */

void v30(int a);
#pragma omp declare variant(v30) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: z) /* { dg-error "'z' is not a function parameter" } */
void b30(int a);

void v31(int a) { (void)a; }
#pragma omp declare variant(v31) match(construct={dispatch}) \
				 adjust_args(need_device_ptr: z) /* { dg-error "'z' is not a function parameter" } */
void b31(int a) { (void)a; }

