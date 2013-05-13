/* Check for thumb1 far jump. This is the extreme case that far jump
 * will be used with minimum number of instructions. By passing this case
 * it means the heuristic of saving lr for far jump meets the most extreme
 * requirement.  */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

volatile register r4 asm("r4");
void f3(int i)
{
#define GO(n) \
  extern volatile int g_##n; \
  r4=(int)&g_##n;

#define GO8(n) \
  GO(n##_0) \
  GO(n##_1) \
  GO(n##_2) \
  GO(n##_3) \
  GO(n##_4) \
  GO(n##_5) \
  GO(n##_6) \
  GO(n##_7)

#define GO64(n) \
  GO8(n##_0) \
  GO8(n##_1) \
  GO8(n##_2) \
  GO8(n##_3) \
  GO8(n##_4) \
  GO8(n##_5) \
  GO8(n##_6) \
  GO8(n##_7) \

#define GO498(n) \
  GO64(n##_0) \
  GO64(n##_1) \
  GO64(n##_2) \
  GO64(n##_3) \
  GO64(n##_4) \
  GO64(n##_5) \
  GO64(n##_6) \
  GO8(n##_0) \
  GO8(n##_1) \
  GO8(n##_2) \
  GO8(n##_3) \
  GO8(n##_4) \
  GO8(n##_5) \
  GO(n##_0) \
  GO(n##_1) \

  if (i) {
    GO498(0);
  }
}

/* { dg-final { scan-assembler "push.*lr" } } */
