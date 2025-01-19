/* Check for thumb1 far jump. This is the extreme case that far jump
 * will be used with minimum number of instructions. By passing this case
 * it means the heuristic of saving lr for far jump meets the most extreme
 * requirement.  */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

volatile int r4;

#define GO() \
  r4 = 1;

#define GO8() \
  GO() \
  GO() \
  GO() \
  GO() \
  GO() \
  GO() \
  GO() \
  GO()

#define GO32() \
  GO8() \
  GO8() \
  GO8() \
  GO8()

#define GO128() \
  GO32() \
  GO32() \
  GO32() \
  GO32()

#define GO512() \
  GO128() \
  GO128() \
  GO128() \
  GO128()

#define GO1018() \
  GO512() \
  GO128() \
  GO128() \
  GO128() \
  GO32() \
  GO32() \
  GO32() \
  GO8() \
  GO8() \
  GO8() \
  GO() \
  GO()

void f3(int i)
{
  GO();
  if (i) {
    GO1018();
  }
}

/* { dg-final { scan-assembler "\tpush.*lr" } } */
/* { dg-final { scan-assembler "\tbl\t\\.L\[0-9\]+\t@far jump" } } */
