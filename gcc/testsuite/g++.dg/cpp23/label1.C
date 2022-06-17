// P2324R2 - Labels at the end of compound statements
// PR c++/103539
// { dg-do compile }
// Test good cases.

void
p2324 ()
{
first:
  int x; 
second:
  x = 1;
last:
} // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }

void
fn1 ()
{
  l1:
} // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }

void
fn2 ()
{
  if (1)
    {
l1:
    } // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }
}

void
fn3 ()
{
  {
    {
label:
    } // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }
  }
}

void
fn4 ()
{
  switch (1)
    {
lab:
    } // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }
}

void
fn5 ()
{
l1:
l2:
l3:
} // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }

void
fn6 ()
{
  ;
l1:
l2:
l3:
} // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }


#if __cplusplus >= 201103L
void
fn7 ()
{
  auto l = [](){
    lab:
  }; // { dg-error "label at end of compound statement only available with" "" { target { c++20_down && c++11 } } }
}
#endif

void
fn8 ()
{
  try
    {
lab1:
    } // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }
  catch (int)
    {
lab2:
    } // { dg-error "label at end of compound statement only available with" "" { target c++20_down } }
}
