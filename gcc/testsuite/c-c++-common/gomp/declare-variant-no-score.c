/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-foffload=disable" } */

void f01 (void);
#pragma omp declare variant (f01) \
	match (device={kind (score(5) : host)})
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-1 } */
void f02 (void);
void f03 (void);
#pragma omp declare variant (f03) \
        match (device={kind (host), arch (score(6) : x86_64), isa (avx512f)})
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-1 } */
void f04 (void);
void f05 (void);
#pragma omp declare variant (f05) \
	match (device={kind (host), arch (score(6) : x86_64),	\
			 isa (score(7): avx512f)})
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 } */
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 } */
void f06 (void);




