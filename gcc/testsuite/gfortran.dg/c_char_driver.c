void param_test(char my_char, char my_char_2);
void sub0(void);
void sub1(char *my_char);

int main(int argc, char **argv)
{
  char my_char = 'y';
  
  param_test('y', 'z');
  sub0();
  sub1(&my_char);
  
  return 0;
}
