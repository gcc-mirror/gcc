/* PR target/38736 */
/* { dg-options "-O2" } */

struct alignment_test_struct
{
  char space[4] __attribute__((__aligned__));
};

extern int aligned_x (void);

int
aligned_x (void)
{
  return __alignof__(struct alignment_test_struct);
}
