// related to PR c++/55189
// { dg-options "-Wreturn-type" }

int f1()
{
  while (true) { }
}
int f2()
{
  while (true) { break; }
} // { dg-warning "no return statement" }

int f3()
{
  for (;;) {}
}
int f4()
{
  for (;;) {break;}
} // { dg-warning "no return statement" }

int f5()
{
  do {} while(true);
}
int f6()
{
  do {break;} while(true);
} // { dg-warning "no return statement" }

int f7()
{
  for(;;)
    while (true) {break;}
}

int f8()
{
  for(;;)
    {
      while (true) {}
      break;
    }
}

template <class T>
T f9()
{
  for(;;) { }
}

template int f9();

template <class T>
T f10()
{
  for(;;) { break; }
} // { dg-warning "no return statement" }

template int f10();
