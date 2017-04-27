/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int main()
{
  long long offset = 10;
  char array[10];
  char c = array[offset];
  return 0;
}

/* { dg-output "index 10 out of bounds for type 'char \\\[10\\\]'" } */
