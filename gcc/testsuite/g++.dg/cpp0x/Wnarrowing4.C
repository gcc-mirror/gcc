// PR c++/66007
// { dg-do run { target c++11 } }
// { dg-options "-Wno-error=narrowing" }

extern "C" void abort();

int main()
{
  unsigned foo[] = { 1, -1, 3 };
  if (foo[0] != 1 || foo[1] != __INT_MAX__ * 2U + 1 || foo[2] != 3)
    abort();
}

// { dg-prune-output "narrowing conversion" }
