#include <altivec.h>

static vector unsigned char select2 = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2};
static vector unsigned char select3 = {4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4};
static vector unsigned char select4 = {8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8};
static vector unsigned char select5 = {16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16};
static vector unsigned char select6 = {32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32};
static vector unsigned char select7 = {64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64};
static vector unsigned char select8 = {128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128};

static vector unsigned char control1
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control2
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control3
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control4
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control5
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control6
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control7
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char control8
  = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
static vector unsigned char rotate1 = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
static vector unsigned char rotate2 = {3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3};
static vector unsigned char rotate3 = {5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5};
static vector unsigned char rotate4 = {7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};
static vector unsigned char rotate5 = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
static vector unsigned char rotate6 = {3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3};
static vector unsigned char rotate7 = {5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5};
static vector unsigned char rotate8 = {7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};

static vector unsigned char permute_128(vector unsigned char input)
{
  vector unsigned char result, new_bit;

  /* and now the code */
  result = vec_vperm(input, input, control1);
  result = vec_rl(result, rotate1);

  new_bit = vec_vperm(input, input, control2);
  new_bit = vec_rl(new_bit, rotate2);
  result = vec_sel(result, new_bit, select2);

  new_bit = vec_vperm(input, input, control3);
  new_bit = vec_rl(new_bit, rotate3);
  result = vec_sel(result, new_bit, select3);

  new_bit = vec_vperm(input, input, control4);
  new_bit = vec_rl(new_bit, rotate4);
  result = vec_sel(result, new_bit, select4);

  new_bit = vec_vperm(input, input, control5);
  new_bit = vec_rl(new_bit, rotate5);
  result = vec_sel(result, new_bit, select5);

  new_bit = vec_vperm(input, input, control6);
  new_bit = vec_rl(new_bit, rotate6);
  result = vec_sel(result, new_bit, select6);

  new_bit = vec_vperm(input, input, control7);
  new_bit = vec_rl(new_bit, rotate7);
  result = vec_sel(result, new_bit, select7);

  new_bit = vec_vperm(input, input, control8);
  new_bit = vec_rl(new_bit, rotate8);
  result = vec_sel(result, new_bit, select8);

  return result;
}

int main()
{
  vector unsigned char input
    = {0,1,2,4,8,16,32,64,128,0,1,2,4,8,16,32};
  vector unsigned char result = permute_128(input);
  return 0;
}

