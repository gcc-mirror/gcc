// Testcase from N2998
// { dg-do compile { target c++11 } }

void f1(int i) {
  int const N = 20;
  auto m1 = [=]{
     int const M = 30;
     auto m2 = [i]{
        int x[N][M]; // OK: N and M are not "used"
        x[0][0] = i; // OK: i is explicitly captured by m2
                     // and implicitly captured by m1
     };
  };
  struct s1 {
    int f;
    void work(int n) {
      int m = n*n;
      int j = 40;
      auto m3 = [this,m]{
        /*auto m4=*/[&,j]{      // { dg-error "j. is not captured" }
          int x = n;	        // { dg-error "n. is not captured" }
          x += m;	        // OK: m implicitly captured by m4
				// and explicitly captured by m3
          x += i;		// { dg-error "i. is not captured" }
          x += f;		// OK: this captured implicitly by m4
				// and explicitly by m3
        };
      };
    }
  };
}
