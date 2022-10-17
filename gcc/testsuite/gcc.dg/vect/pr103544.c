/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=haswell" { target x86_64-*-* i?86-*-* } } */

int crash_me(char* ptr, unsigned long size)
{
  short result[16] = {0};

  unsigned long no_iters = 0;
  for(unsigned long i = 0; i < size - 12; i+= 13){
      for(unsigned long j = 0; j < 12; j++){
	  result[j] += ptr[i + j] - '0';
      }
      no_iters++;
  }

  int result_int = 0;
  for(int j = 0; j < 12; j++){
      int bit_value = result[j] > no_iters/2 ? 1 : 0;
      result_int |= bit_value;
  }

  return result_int;
}
