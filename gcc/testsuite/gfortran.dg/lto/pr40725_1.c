typedef struct c_type_1
{
  int j;
} c_type_1_t;
void sub0(c_type_1_t *c_type, int expected_j);
int main(int argc, char **argv)
{
  c_type_1_t c_type;
  c_type.j = 11;
  sub0(&c_type, c_type.j);
  return 0;
}
