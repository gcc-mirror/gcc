#define DECLARE_ARRAY(A) array##A[0x10]
#define DECLARE_COUNTER(A) counter##A = 0
#define DECLARE_POINTER(A) *pointer##A = array##A + x
/* Create a loop that allows post-modification of pointerA, followed by
   a use of the post-modified address.  */
#define BEFORE(A) counter##A += *pointer##A, pointer##A += 3
#define AFTER(A) counter##A += pointer##A[x]

/* Set up the arrays so that one iteration of the loop sets the counter
   to 3.0f.  */
#define INIT_ARRAY(A) array##A[1] = 1.0f, array##A[5] = 2.0f

/* Check that the loop worked correctly for all values.  */
#define CHECK_ARRAY(A) exit_code |= (counter##A != 3.0f)

/* Having 6 copies triggered the bug for ARM and Thumb.  */
#define MANY(A) A (0), A (1), A (2), A (3), A (4), A (5)

/* Each addendA should be allocated a register.  */
#define INIT_VOLATILE(A) addend##A = vol
#define ADD_VOLATILE(A) vol += addend##A

/* Having 5 copies triggered the bug for ARM and Thumb.  */
#define MANY2(A) A (0), A (1), A (2), A (3), A (4)

float MANY (DECLARE_ARRAY);
float MANY (DECLARE_COUNTER);

volatile int stop = 1;
volatile int vol;

void __attribute__((noinline))
foo (int x)
{
  float MANY (DECLARE_POINTER);
  int i;

  do
    {
      MANY (BEFORE);
      MANY (AFTER);
      /* Create an inner loop that should ensure the code above
	 has registers free for reload inheritance.  */
      {
	int MANY2 (INIT_VOLATILE);
	for (i = 0; i < 10; i++)
	  MANY2 (ADD_VOLATILE);
      }
    }
  while (!stop);
}

int
main (void)
{
  int exit_code = 0;

  MANY (INIT_ARRAY);
  foo (1);
  MANY (CHECK_ARRAY);
  return exit_code;
}
