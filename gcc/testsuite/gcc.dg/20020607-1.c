/* PR middle-end/6950
   gcc 3.0.4 mistakenly set lhs.low to 0 at the beginning of the num_eq
   expansion; it should use a temporary.
/* { dg-do run } */

typedef struct cpp_num cpp_num;
struct cpp_num
{
  long high;
  long low;
  char overflow;
};

#define num_eq(num1, num2) (num1.low == num2.low && num1.high == num2.high)

static cpp_num
num_equality_op (lhs, rhs)
     cpp_num lhs, rhs;
{
  lhs.low = num_eq (lhs, rhs);
  lhs.high = 0;
  lhs.overflow = 0;
  return lhs;
}

int main()
{
  cpp_num a = { 1, 2 };
  cpp_num b = { 3, 4 };

  cpp_num result = num_equality_op (a, b);
  if (result.low)
    return 1;

  result = num_equality_op (a, a);
  if (!result.low)
    return 2;

  return 0;
}
