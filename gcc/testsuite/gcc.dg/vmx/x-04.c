#include <altivec.h>
vector unsigned char
permute_128(vector unsigned char input)
{
  vector unsigned char result, new_bit;

  vector unsigned char select2 = ((vector unsigned char){2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2});
  vector unsigned char select3 = ((vector unsigned char){4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4});
  vector unsigned char select4 = ((vector unsigned char){8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8});
  vector unsigned char select5 = ((vector unsigned char){16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16});
  vector unsigned char select6 = ((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32});
  vector unsigned char select7 = ((vector unsigned char){64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64});
  vector unsigned char select8 = ((vector unsigned char){128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128});

  vector unsigned char control1
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control2
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control3
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control4
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control5
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control6
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control7
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char control8
    = ((vector unsigned char){15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0});
  vector unsigned char rotate1 = ((vector unsigned char){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1});
  vector unsigned char rotate2 = ((vector unsigned char){3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3});
  vector unsigned char rotate3 = ((vector unsigned char){5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5});
  vector unsigned char rotate4 = ((vector unsigned char){7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7});
  vector unsigned char rotate5 = ((vector unsigned char){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1});
  vector unsigned char rotate6 = ((vector unsigned char){3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3});
  vector unsigned char rotate7 = ((vector unsigned char){5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5});
  vector unsigned char rotate8 = ((vector unsigned char){7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7});

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
    = ((vector unsigned char){0,1,2,4,8,16,32,64,128,0,1,2,4,8,16,32});
  vector unsigned char result = permute_128(input);
  return 0;
}
