/* Test OpenACC 'routine' with 'nohost' clause, invalid use.  */

#pragma acc routine /* { dg-note {\.\.\. without 'nohost' clause near to here} } */
int THREE_1(void)
{
  return 3;
}

#pragma acc routine (THREE_1) \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*THREE_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */

#pragma acc routine \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*THREE_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern int THREE_1(void);


#pragma acc routine /* { dg-note {\.\.\. without 'nohost' clause near to here} } */
extern void NOTHING_1(void);

#pragma acc routine (NOTHING_1) \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */

void NOTHING_1(void)
{
}

#pragma acc routine \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern void NOTHING_1(void);

#pragma acc routine (NOTHING_1) \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */


extern float ADD_1(float, float);

#pragma acc routine (ADD_1) /* { dg-note {\.\.\. without 'nohost' clause near to here} } */

float ADD_1(float x, float y)
{
  return x + y;
}

#pragma acc routine \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*ADD_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern float ADD_1(float, float);

#pragma acc routine (ADD_1) \
  nohost /* { dg-error {incompatible 'nohost' clause when applying '#pragma acc routine' to '[^']*ADD_1[^']*', which has already been marked with an OpenACC 'routine' directive} } */


/* The same again, but with/without nohost reversed.  */

#pragma acc routine \
  nohost /* { dg-note {\.\.\. with 'nohost' clause here} } */
int THREE_2(void)
{
  return 3;
}

#pragma acc routine (THREE_2) /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*THREE_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */

#pragma acc routine /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*THREE_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern int THREE_2(void);


#pragma acc routine \
  nohost /* { dg-note {\.\.\. with 'nohost' clause here} } */
extern void NOTHING_2(void);

#pragma acc routine (NOTHING_2) /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */

void NOTHING_2(void)
{
}

#pragma acc routine /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern void NOTHING_2(void);

#pragma acc routine (NOTHING_2) /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*NOTHING_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */


extern float ADD_2(float, float);

#pragma acc routine (ADD_2) \
  nohost /* { dg-note {\.\.\. with 'nohost' clause here} } */

float ADD_2(float x, float y)
{
  return x + y;
}

#pragma acc routine /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*ADD_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */
extern float ADD_2(float, float);

#pragma acc routine (ADD_2) /* { dg-error {missing 'nohost' clause when applying '#pragma acc routine' to '[^']*ADD_2[^']*', which has already been marked with an OpenACC 'routine' directive} } */
