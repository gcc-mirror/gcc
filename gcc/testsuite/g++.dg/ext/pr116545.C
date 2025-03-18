// PR c/116545
// { dg-do compile }
// { dg-options "-Wall" }

void
foo (bool x)
{
  [[]] __attribute__(()) [[]] __attribute__((assume (true))) [[]];
  [[]] __attribute__(()) [[assume (true)]] __attribute__(()) [[]];
  __attribute__(()) [[]] __attribute__((assume (true))) [[]] __attribute__(());
  __attribute__(()) [[assume (true)]] __attribute__(()) [[]] __attribute__(());
  if (__attribute__((assume (true))); x)	// { dg-warning "init-statement in selection statements only available with" "" { target c++14_down } }
    ;
}

void
bar (int x)
{
  switch (x)
    {
    case 1:
      ++x;
      [[]] __attribute__(()) [[]] __attribute__((fallthrough)) [[]];
    case 2:
      ++x;
      [[]] __attribute__(()) [[fallthrough]] __attribute__(()) [[]];
    case 3:
      ++x;
      __attribute__(()) [[]] __attribute__((__fallthrough__)) [[]] __attribute__(());
    case 4:
      ++x;
      __attribute__(()) [[fallthrough]] __attribute__(()) [[]] __attribute__(());
    case 5:
      ++x;
      break;
    default:
      break;
    }
}
