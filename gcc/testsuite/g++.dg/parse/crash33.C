// PR c++/28878
// { dg-do compile }

template<int>
void foo()
[
  throw;	// { dg-error "expected" }
}		// { dg-error "expected" }
