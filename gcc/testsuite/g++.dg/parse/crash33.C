// PR c++/28878
// { dg-do compile }

template<int>
void foo()
[
  throw;
}

// { dg-prune-output "expected" }
// { dg-prune-output "array bound" }
