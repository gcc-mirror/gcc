/* { dg-require-stack-size "1024" } */

int main ()
{
  char temp[1024] = "tempfile";
  return temp[0] != 't';
}

