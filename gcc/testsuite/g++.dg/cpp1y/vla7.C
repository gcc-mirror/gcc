// PR c++/55520
// { dg-options "-Wno-vla" }
// { dg-require-effective-target c++11 }

int main(int argc, char** argv)
{
  int x[1][argc];

  [&x](int i) {			// { dg-prune-output "sorry" }
    x[0][i] = 0;	     	// { dg-prune-output "not captured" }
  }(5);
}
