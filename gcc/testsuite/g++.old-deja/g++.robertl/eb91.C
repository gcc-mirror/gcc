#include <sys/types.h>
#include <algorithm>
typedef short int16_t;
typedef unsigned short u_int16_t;

template <class INT>
class other_endian
{
private:
   
  INT value;

   
  u_int16_t change_endian(u_int16_t x)
  {
    union {
      u_int16_t i;
      u_int8_t c[2];
    } val;
    val.i = x;
    swap(val.c[0], val.c[1]);
    return val.i;
  };

  int16_t change_endian(int16_t x)
  {
    union {
      int16_t i;
      int8_t c[2];
    } val;
    val.i = x;
    swap(val.c[0], val.c[1]);
    return val.i;
  };
public:
  other_endian(const INT i = 0)
  {
    value = change_endian(i);
  }

  operator INT()
  {
    return change_endian(value);
  }
};

template <class INT>
class same_endian
{
  INT value;

public:
  same_endian(const INT i = 0)
  {
    value = i;
  }

  operator INT()
  {
    return value;
  }
};

















int main() {
  other_endian <u_int16_t> little_endian_16_bit_int;
  return 0;
}
