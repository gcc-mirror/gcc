/* Test various aspects of clauses specifying incompatible levels of
   parallelism with the OpenACC 'routine' directive.  */

extern void g_1 (void);
#pragma acc routine (g_1) gang gang /* { dg-error "too many 'gang' clauses" } */

#pragma acc routine worker worker /* { dg-error "too many 'worker' clauses" } */
void w_1 (void)
{
}

#pragma acc routine vector vector /* { dg-error "too many 'vector' clauses" } */
void v_1 (void)
{
}

#pragma acc routine seq seq /* { dg-error "too many 'seq' clauses" } */
extern void s_1 (void);


#pragma acc routine gang gang gang /* { dg-error "too many 'gang' clauses" } */
void g_2 (void)
{
}

#pragma acc routine worker worker worker /* { dg-error "too many 'worker' clauses" } */
extern void w_2 (void);

extern void v_2 (void);
#pragma acc routine (v_2) vector vector vector /* { dg-error "too many 'vector' clauses" } */

#pragma acc routine seq seq seq /* { dg-error "too many 'seq' clauses" } */
void s_2 (void)
{
}


#pragma acc routine \
  gang \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */
void g_3 (void)
{
}
#pragma acc routine (g_3) \
  gang \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */
#pragma acc routine (g_3) \
  gang \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */

extern void w_3 (void);
#pragma acc routine (w_3) \
  worker \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */
#pragma acc routine (w_3) \
  worker \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
#pragma acc routine (w_3) \
  worker \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */

#pragma acc routine \
  vector \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */
void v_3 (void)
{
}
#pragma acc routine (v_3) \
  vector \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */
#pragma acc routine (v_3) \
  vector \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */

extern void s_3 (void);
#pragma acc routine (s_3) \
  seq \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
#pragma acc routine (s_3) \
  seq \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */
#pragma acc routine (s_3) \
  seq \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */


#pragma acc routine \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */
extern void g_4 (void);
#pragma acc routine (g_4) \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */
#pragma acc routine (g_4) \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */

extern void w_4 (void);
#pragma acc routine (w_4) \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
#pragma acc routine (w_4) \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
#pragma acc routine (w_4) \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */

#pragma acc routine \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
void v_4 (void)
{
}
#pragma acc routine (v_4) \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
#pragma acc routine (v_4) \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */ \
  seq /* { dg-error ".seq. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */

#pragma acc routine \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */
void s_4 (void)
{
}
#pragma acc routine (s_4) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */
#pragma acc routine (s_4) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  worker /* { dg-error ".worker. specifies a conflicting level of parallelism" } */ \
  vector /* { dg-error ".vector. specifies a conflicting level of parallelism" } */ \
  gang /* { dg-error ".gang. specifies a conflicting level of parallelism" } */


#pragma acc routine \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
void g_5 (void)
{
}
#pragma acc routine (g_5) \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (g_5) \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

#pragma acc routine \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
extern void w_5 (void);
#pragma acc routine (w_5) \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (w_5) \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

#pragma acc routine \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
extern void v_5 (void);
#pragma acc routine (v_5) \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (v_5) \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

extern void s_5 (void);
#pragma acc routine (s_5) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (s_5) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (s_5) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */


/* Like the *_5 tests, but with the order of clauses changed in the second and
   following routine directives for the specific *_5 function.  */

#pragma acc routine \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
void g_6 (void)
{
}
#pragma acc routine (g_6) \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*g_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (g_6) \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*g_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  gang gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

#pragma acc routine \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
extern void w_6 (void);
#pragma acc routine (w_6) \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*w_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (w_6) \
  seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*w_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

#pragma acc routine \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
extern void v_6 (void);
#pragma acc routine (v_6) \
  seq seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*v_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (v_6) \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error "incompatible .gang. clause when applying .#pragma acc routine. to .\[void \]*v_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  seq seq seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */

extern void s_6 (void);
#pragma acc routine (s_6) \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (s_6) \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*s_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  worker worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error ".worker. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */
#pragma acc routine (s_6) \
  worker worker /* { dg-error "too many 'worker' clauses" } */ \
  /* { dg-error "incompatible .worker. clause when applying .#pragma acc routine. to .\[void \]*s_6\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" "" { target *-*-* } .-1 } */ \
  seq seq seq /* { dg-error "too many 'seq' clauses" } */ \
  /* { dg-error ".seq. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  vector vector vector vector /* { dg-error "too many 'vector' clauses" } */ \
  /* { dg-error ".vector. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */ \
  gang gang /* { dg-error "too many 'gang' clauses" } */ \
  /* { dg-error ".gang. specifies a conflicting level of parallelism" "" { target *-*-* } .-1 } */


/* Like the *_6 tests, but without all the duplicate clauses, so that the
   routine directives are valid in isolation.  */

#pragma acc routine \
  gang
void g_7 (void)
{
}
#pragma acc routine (g_7) \
  vector /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*g_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (g_7) \
  seq /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*g_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

#pragma acc routine \
  worker
extern void w_7 (void);
#pragma acc routine (w_7) \
  vector /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*w_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (w_7) \
  seq /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*w_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

#pragma acc routine \
  vector
extern void v_7 (void);
#pragma acc routine (v_7) \
  seq /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*v_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (v_7) \
  gang /* { dg-error "incompatible .gang. clause when applying .#pragma acc routine. to .\[void \]*v_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

extern void s_7 (void);
#pragma acc routine (s_7) \
  seq
#pragma acc routine (s_7) \
  vector /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*s_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (s_7) \
  worker /* { dg-error "incompatible .worker. clause when applying .#pragma acc routine. to .\[void \]*s_7\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */


/* Test cases for implicit seq clause.  */

#pragma acc routine \
  gang
void g_8 (void)
{
}
#pragma acc routine (g_8) /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*g_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

#pragma acc routine \
  worker
extern void w_8 (void);
#pragma acc routine (w_8) /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*w_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

#pragma acc routine \
  vector
extern void v_8 (void);
#pragma acc routine (v_8) /* { dg-error "incompatible .seq. clause when applying .#pragma acc routine. to .\[void \]*v_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */

extern void s_8 (void);
#pragma acc routine (s_8)
#pragma acc routine (s_8) \
  vector /* { dg-error "incompatible .vector. clause when applying .#pragma acc routine. to .\[void \]*s_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (s_8) \
  gang /* { dg-error "incompatible .gang. clause when applying .#pragma acc routine. to .\[void \]*s_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
#pragma acc routine (s_8) \
  worker /* { dg-error "incompatible .worker. clause when applying .#pragma acc routine. to .\[void \]*s_8\[\\(\\)\]*., which has already been marked with an OpenACC 'routine' directive" } */
