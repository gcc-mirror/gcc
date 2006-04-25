// PR c++/16376
// { dg-do run }

int main(void){
  struct bits {
    unsigned int ui3 : 3;
  } bits;
  int i = -1;   /* is a very large positive number as unsigned */

  bits.ui3 = 1u;
  if( bits.ui3 < i )
    return 1;
  return 0;
}
