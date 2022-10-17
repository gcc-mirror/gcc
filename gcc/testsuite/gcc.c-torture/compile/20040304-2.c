/* PR optimization/14235 */
/* Origin: <senor_fjord@yahoo.com> */

typedef signed char        int8_t;
typedef short              int16_t;
typedef int                int32_t;
typedef unsigned long long uint64_t;

static const uint64_t LOW_BYTE_MASK    = 0x00000000000000ffULL;
static const uint64_t HIGH_BYTE_MASK   = 0x000000000000ff00ULL;
static const uint64_t WORD_MASK        = 0x000000000000ffffULL;
static const uint64_t DWORD_MASK       = 0x00000000ffffffffULL;

extern uint64_t *srca_mask;
extern int *assert_thrown;

void foo()
{
  uint64_t tempA = 0; /* actually a bunch of code to set A */ 
  uint64_t tempB = 0; /* actually a bunch of code to set B */ 

  /* cast A to right size */
  tempA = (((*srca_mask == LOW_BYTE_MASK) || 
            (*srca_mask == HIGH_BYTE_MASK)) ?
           ((int8_t)tempA) : 
           ((*srca_mask == WORD_MASK) ? 
            ((int16_t)tempA) : 
            ((*srca_mask == DWORD_MASK) ? 
             ((int32_t)tempA) : 
             tempA)));

  /* cast B to right size */
  tempB = (((*srca_mask == LOW_BYTE_MASK) || 
            (*srca_mask == HIGH_BYTE_MASK)) ? 
           ((int8_t)tempB) : 
           ((*srca_mask == WORD_MASK) ? 
            ((int16_t)tempB) : 
            ((*srca_mask == DWORD_MASK) ? 
             ((int32_t)tempB) : 
             tempB))); 
    
  if ((int) tempA > (int) tempB) { 
    *assert_thrown = 1;
  }
}
