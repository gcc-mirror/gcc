// { dg-do run { target c++11 } }

// Test catching as pointer and pointer to member types, [except.handle] p3.

extern "C" void abort (void);

typedef decltype(nullptr) nullptr_t;

int result = 0;

void __attribute__((noinline))
caught(int bit)
{
  result |= bit;
}

struct A { };

int main()
{
  try {
    try {
      try {
        try {
          try {
            throw nullptr;
          } catch (void* p) {
            if (p == nullptr)
              caught(1);
            throw;
          }
        } catch (void(*pf)()) {
          if (pf == nullptr)
            caught(2);
          throw;
        }
      } catch (int A::*pm) {
        if (pm == nullptr)
          caught(4);
        throw;
      }
    } catch (int (A::*pmf)()) {
      if (pmf == nullptr)
        caught(8);
      throw;
    }
  } catch (nullptr_t) {
  }

  try {
    try {
      try {
        try {
          try {
            throw nullptr;
          } catch (void* const& p) {
            if (p == nullptr)
              caught(16);
            throw;
          }
        } catch (void(* const& pf)()) {
          if (pf == nullptr)
            caught(32);
          throw;
        }
      } catch (int A::* const& pm) {
        if (pm == nullptr)
          caught(64);
        throw;
      }
    } catch (int (A::* const& pmf)()) {
      if (pmf == nullptr)
        caught(128);
      throw;
    }
  } catch (nullptr_t) {
  }

  if (result != 255)
    abort ();
}
