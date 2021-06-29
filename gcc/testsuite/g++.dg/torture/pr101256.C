// { dg-do run }

template<class T> 
const T& max(const T& a, const T& b)
{
    return (a < b) ? b : a;
}

signed char var_5 = -128;
unsigned int var_11 = 2144479212U;
unsigned long long int arr [22];

void
__attribute__((noipa))
test(signed char var_5, unsigned var_11) {
  for (short i_61 = 0; i_61 < var_5 + 149; i_61 += 10000)
    arr[i_61] = max((signed char)0, var_5) ? max((signed char)1, var_5) : var_11;
}

int main() {
  for (int i_0 = 0; i_0 < 22; ++i_0) 
      arr [i_0] = 11834725929543695741ULL;

  test(var_5, var_11);
  if (arr [0] != 2144479212ULL && arr [0] != 11834725929543695741ULL)
    __builtin_abort ();
  return 0;
}
