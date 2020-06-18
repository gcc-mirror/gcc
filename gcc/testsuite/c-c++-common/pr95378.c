/* { dg-do compile } */

#define seq_cst __ATOMIC_SEQ_CST

extern int *i;
extern int *j;
extern const int *c;
extern volatile int *v;
extern const volatile int *cv;

void
load()
{
  __atomic_load(c, i, seq_cst);
  __atomic_load(cv, i, seq_cst);

  __atomic_load(i, c, seq_cst);
  /* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_load' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_load(i, v, seq_cst);
  /* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_load' discards 'volatile' qualifier" "" { target c } .-2 } */
  __atomic_load(i, cv, seq_cst);
  /* { dg-error "argument 2 of '__atomic_load' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_load' discards 'const' qualifier" "" { target c } .-2 } */
  /* { dg-warning "argument 2 of '__atomic_load' discards 'volatile' qualifier" "" { target c } .-3 } */
}

void
store()
{
  __atomic_store(i, c, seq_cst);
  __atomic_store(v, c, seq_cst);

  __atomic_store(c, i, seq_cst);
  /* { dg-error "argument 1 of '__atomic_store' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_store' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_store(cv, i, seq_cst);
  /* { dg-error "argument 1 of '__atomic_store' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_store' discards 'const' qualifier" "" { target c } .-2 } */

  __atomic_store(i, v, seq_cst);
  /* { dg-error "argument 2 of '__atomic_store' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_store' discards 'volatile' qualifier" "" { target c } .-2 } */
}

void
exchange()
{
  __atomic_exchange(i, c, j, seq_cst);
  __atomic_exchange(v, i, j, seq_cst);
  __atomic_exchange(v, c, j, seq_cst);

  __atomic_exchange(c, i, j, seq_cst);
  /* { dg-error "argument 1 of '__atomic_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_exchange' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_exchange(cv, i, j, seq_cst);
  /* { dg-error "argument 1 of '__atomic_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_exchange' discards 'const' qualifier" "" { target c } .-2 } */

  __atomic_exchange(i, v, j, seq_cst);
  /* { dg-error "argument 2 of '__atomic_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */
  __atomic_exchange(i, cv, j, seq_cst);
  /* { dg-error "argument 2 of '__atomic_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */

  __atomic_exchange(i, j, c, seq_cst);
  /* { dg-error "argument 3 of '__atomic_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 3 of '__atomic_exchange' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_exchange(i, j, v, seq_cst);
  /* { dg-error "argument 3 of '__atomic_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 3 of '__atomic_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */
  __atomic_exchange(i, j, cv, seq_cst);
  /* { dg-error "argument 3 of '__atomic_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 3 of '__atomic_exchange' discards 'const' qualifier" "" { target c } .-2 } */
  /* { dg-warning "argument 3 of '__atomic_exchange' discards 'volatile' qualifier" "" { target c } .-3 } */
}

void
compare_exchange()
{
  __atomic_compare_exchange(i, j, c, 1, seq_cst, seq_cst);
  __atomic_compare_exchange(v, i, j, 1, seq_cst, seq_cst);
  __atomic_compare_exchange(v, i, c, 1, seq_cst, seq_cst);

  __atomic_compare_exchange(c, i, j, 1, seq_cst, seq_cst);
  /* { dg-error "argument 1 of '__atomic_compare_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_compare_exchange' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_compare_exchange(cv, i, j, 1, seq_cst, seq_cst);
  /* { dg-error "argument 1 of '__atomic_compare_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 1 of '__atomic_compare_exchange' discards 'const' qualifier" "" { target c } .-2 } */

  __atomic_compare_exchange(i, c, j, 1, seq_cst, seq_cst);
  /* { dg-error "argument 2 of '__atomic_compare_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_compare_exchange' discards 'const' qualifier" "" { target c } .-2 } */
  __atomic_compare_exchange(i, v, j, 1, seq_cst, seq_cst);
  /* { dg-error "argument 2 of '__atomic_compare_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_compare_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */
  __atomic_compare_exchange(i, cv, j, 1, seq_cst, seq_cst);
  /* { dg-error "argument 2 of '__atomic_compare_exchange' must not be a pointer to a 'const' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 2 of '__atomic_compare_exchange' discards 'const" "" { target c } .-2 } */
  /* { dg-warning "argument 2 of '__atomic_compare_exchange' discards 'volatile' qualifier" "" { target c } .-3 } */

  __atomic_compare_exchange(i, j, v, 1, seq_cst, seq_cst);
  /* { dg-error "argument 3 of '__atomic_compare_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 3 of '__atomic_compare_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */
  __atomic_compare_exchange(i, j, cv, 1, seq_cst, seq_cst);
  /* { dg-error "argument 3 of '__atomic_compare_exchange' must not be a pointer to a 'volatile' type" "" { target c++ } .-1 } */
  /* { dg-warning "argument 3 of '__atomic_compare_exchange' discards 'volatile' qualifier" "" { target c } .-2 } */
}
