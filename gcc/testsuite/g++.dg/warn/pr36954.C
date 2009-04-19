// PR c++/36954
// { dg-do compile }
// { dg-options "-Wlogical-op -Wextra -Wall" }

template<class C> void Test()
{
  if ((1 == 2) || (true)) { 
  }

  if ((1 == 2) || (!false)) {
  }

  if (false || true) {
  }
}



int main() {
  if ((1 == 2) || (true)) {
  }
}

