/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O3" } */

int vec[500];

void func()
{ 
  __transaction_relaxed
    { 
      vec[123] = 456;
    }
}

main()
{ 
  int i;
  for(i = 0; i < 10; ++i)
    func();
}
