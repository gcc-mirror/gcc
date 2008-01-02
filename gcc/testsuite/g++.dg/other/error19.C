// PR c++/33495

void foo()
{
  if (({while(true);})) // { dg-error "forbids|<statement>" }
    ;
}
