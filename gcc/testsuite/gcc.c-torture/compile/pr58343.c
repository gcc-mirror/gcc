int a;

int main ()
{
  int b = a; 

  for (a = 1; a > 0; a--)
    ;

 lbl:
  if (b && a)
    goto lbl; 

  return 0;
}
