/* { dg-do run } */
/* PR middle-end/116772  */
/* The division by `/b` should not
   be made uncondtional. */

int mult0(int a,int b) __attribute__((noipa));

int mult0(int a,int b){
  return (b!=0 ? (a/b)*b : 0);
}

int bit_and0(int a,int b) __attribute__((noipa));

int bit_and0(int a,int b){
  return (b!=0 ? (a/b)&b : 0);
}

int main() {
  if (mult0(3, 0) != 0)
    __builtin_abort();
  if (bit_and0(3, 0) != 0)
    __builtin_abort();
  return 0;
}
