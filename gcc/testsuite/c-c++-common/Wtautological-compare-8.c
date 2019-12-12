/* { dg-options "-Wtautological-compare" } */

int foo;
#define INCOMING_FRAME_SP_OFFSET foo
#define DEFAULT_INCOMING_FRAME_SP_OFFSET INCOMING_FRAME_SP_OFFSET

int test (void)
{
  if (DEFAULT_INCOMING_FRAME_SP_OFFSET != INCOMING_FRAME_SP_OFFSET) /* { dg-warning "self-comparison" "" { target c } } */
    return 1;
  else
    return 0;
}

#define BYTES_BIG_ENDIAN foo
#define WORDS_BIG_ENDIAN foo

int test_2 (void)
{
  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN) /* { dg-warning "self-comparison" "" { target c } } */
    return 1;
  else
    return 0;
}

#define COND DEFAULT_INCOMING_FRAME_SP_OFFSET != INCOMING_FRAME_SP_OFFSET
int test_3 (void)
{
  if (COND)
    return 1;
  else
    return 0;
}
