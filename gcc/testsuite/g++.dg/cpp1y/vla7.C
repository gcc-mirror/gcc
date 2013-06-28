// PR c++/55520
// { dg-options "-Wno-vla" }
// { dg-require-effective-target c++11 }

int main(int argc, char** argv)
{
  int x[1][argc];

  [&x](int i) {			// { dg-error "variable.size" }
    x[0][i] = 0;	     	// { dg-prune-output "assignment" }
  }(5);
}
