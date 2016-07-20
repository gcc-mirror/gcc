// PR c++/71909
// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

struct S
{
  S () __transaction_atomic [[outer]] try : m {0} {} catch (int) {} catch (...) {}
  int m;
};

struct T
{
  T () __transaction_atomic __attribute__((outer)) try : m {0} {} catch (int) {} catch (...) {}
  int m;
};

void foo () __transaction_atomic [[outer]] try {} catch (int) {} catch (...) {}
void bar () __transaction_atomic __attribute__((outer)) try {} catch (int) {} catch (...) {}
