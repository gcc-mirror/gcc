// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis when taking address of an unqualified xobj member function

// { dg-message "In explicit object member function" "" { target *-*-* } 0 }

struct S {
  void f(this S&) {}

  void g(this S&) {}
  void g(this S&, int) {}

  void test0() {
    void (*fp)(S&) = &f; // { dg-line line_sf }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } line_sf }
    // { dg-note {a pointer to explicit object member function can only be formed with '&S::f'} "" { target *-*-* } line_sf }
    void (*gp)(S&) = &g; // { dg-line line_sg }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } line_sg }
    // { dg-note {a pointer to explicit object member function can only be formed with '&S::g'} "" { target *-*-* } line_sg }
  }

  void test1(this S& self) {
    void (*fp)(S&) = &self.f; // { dg-line s_test1_f }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } s_test1_f }
    // { dg-note {a pointer to explicit object member function can only be formed with '&S::f'} "" { target *-*-* } s_test1_f }
    void (*gp)(S&) = &self.g; // { dg-line s_test1_g }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } s_test1_g }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } s_test1_g }
    // { dg-note {a pointer to explicit object member function can only be formed with '&S::g'} "" { target *-*-* } s_test1_g }
  }
};

void test0()
{
  S s{};

  void (*fp)(S&) = &s.f; // { dg-line s_free_test0_f }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } s_free_test0_f }
  // { dg-note {a pointer to explicit object member function can only be formed with '&S::f'} "" { target *-*-* } s_free_test0_f }
  void (*gp)(S&) = &s.g; // { dg-line s_free_test0_g }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } s_free_test0_g }
  // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } s_free_test0_g }
  // { dg-note {a pointer to explicit object member function can only be formed with '&S::g'} "" { target *-*-* } s_free_test0_g }
}

struct D;

struct B {
  void fb(this B&) {}

  void gb(this B&) {}
  void gb(this B&, int) {}

  void fd(this D&) {}

  void gd(this D&) {}
  void gd(this D&, int) {}
};

struct D : B {
  void fb2(this B&) {}

  void gb2(this B&) {}
  void gb2(this B&, int) {}

  void fd2(this D&) {}

  void gd2(this D&) {}
  void gd2(this D&, int) {}

  void test0() {
    void (*fbp)(B&) = &fb; // { dg-line d_test0_fb }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test0_fb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb'} "" { target *-*-* } d_test0_fb }
    void (*gbp)(B&) = &gb; // { dg-line d_test0_gb }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test0_gb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb'} "PR113075" { xfail *-*-* } d_test0_gb }
    // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gb'} "PR113075" { xfail *-*-* } d_test0_gb }

    void (*fdp)(D&) = &fd; // { dg-line d_test0_fd }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test0_fd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd'} "" { target *-*-* } d_test0_fd }
    void (*gdp)(D&) = &gd; // { dg-line d_test0_gd }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test0_gd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd'} "PR113075" { xfail *-*-* } d_test0_gd }
    // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gd'} "PR113075" { xfail *-*-* } d_test0_gd }
  }

  void test1(this B& self) {
    void (*fbp)(B&) = &self.fb; // { dg-line d_test1_fb }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test1_fb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&B::fb'} "" { target *-*-* } d_test1_fb }
    void (*gbp)(B&) = &self.gb; // { dg-line d_test1_gb }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test1_gb }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test1_gb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&B::gb'} "" { target *-*-* } d_test1_gb }

    void (*fdp)(D&) = &self.fd; // { dg-line d_test1_fd }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test1_fd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&B::fd'} "" { target *-*-* } d_test1_fd }
    void (*gdp)(D&) = &self.gd; // { dg-line d_test1_gd }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test1_gd }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test1_gd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&B::gd'} "" { target *-*-* } d_test1_gd }
  }

  void test2(this D& self) {
    void (*fbp)(B&) = &self.fb; // { dg-line d_test2_fb }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test2_fb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb'} "" { target *-*-* } d_test2_fb }
    void (*gbp)(B&) = &self.gb; // { dg-line d_test2_gb }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test2_gb }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test2_gb }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb'} "PR113075" { xfail *-*-* } d_test2_gb }
    // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gb'} "PR113075" { xfail *-*-* } d_test2_gb }

    void (*fdp)(D&) = &self.fd; // { dg-line d_test2_fd }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test2_fd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd'} "" { target *-*-* } d_test2_fd }
    void (*gdp)(D&) = &self.gd; // { dg-line d_test2_gd }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test2_gd }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test2_gd }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd'} "PR113075" { xfail *-*-* } d_test2_gd }
    // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gd'} "PR113075" { xfail *-*-* } d_test2_gd }
  }

  void test3() {
    void (*fbp)(B&) = &fb2; // { dg-line d_test3_fb2 }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test3_fb2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb2'} "" { target *-*-* } d_test3_fb2 }
    void (*gbp)(B&) = &gb2; // { dg-line d_test3_gb2 }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test3_gb2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb2'} "" { target *-*-* } d_test3_gb2 }

    void (*fdp)(D&) = &fd2; // { dg-line d_test3_fd2 }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test3_fd2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd2'} "" { target *-*-* } d_test3_fd2 }
    void (*gdp)(D&) = &gd2; // { dg-line d_test3_gd2 }
    // { dg-error {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "" { target *-*-* } d_test3_gd2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd2'} "" { target *-*-* } d_test3_gd2 }
  }

  void test4(this D& self) {
    void (*fbp)(B&) = &self.fb2; // { dg-line d_test4_fb2 }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test4_fb2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb2'} "" { target *-*-* } d_test4_fb2 }
    void (*gbp)(B&) = &self.gb2; // { dg-line d_test4_gb2 }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test4_gb2 }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test4_gb2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb2'} "" { target *-*-* } d_test4_gb2 }

    void (*fdp)(D&) = &self.fd2; // { dg-line d_test4_fd2 }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_test4_fd2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd2'} "" { target *-*-* } d_test4_fd2 }
    void (*gdp)(D&) = &self.gd2; // { dg-line d_test4_gd2 }
    // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test4_gd2 }
    // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_test4_gd2 }
    // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd2'} "" { target *-*-* } d_test4_gd2 }
  }
};

void test1()
{
  D d{};

  void (*fbp)(B&) = &d.fb; // { dg-line d_free_test1_fb }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_free_test1_fb }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb'} "" { target *-*-* } d_free_test1_fb }
  void (*gbp)(B&) = &d.gb; // { dg-line d_free_test1_gb }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test1_gb }
  // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test1_gb }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb'} "PR113075" { xfail *-*-* } d_free_test1_gb }
  // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gb'} "PR113075" { xfail *-*-* } d_free_test1_gb }

  void (*fdp)(D&) = &d.fd; // { dg-line d_free_test1_fd }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_free_test1_fd }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd'} "" { target *-*-* } d_free_test1_fd }
  void (*gdp)(D&) = &d.gd; // { dg-line d_free_test1_gd }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test1_gd }
  // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test1_gd }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd'} "PR113075" { xfail *-*-* } d_free_test1_gd }
  // { dg-bogus {a pointer to explicit object member function can only be formed with '&B::gd'} "PR113075" { xfail *-*-* } d_free_test1_gd }
}

void test2()
{
  D d{};

  void (*fbp)(B&) = &d.fb2; // { dg-line d_free_test2_fb2 }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_free_test2_fb2 }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::fb2'} "" { target *-*-* } d_free_test2_fb2 }
  void (*gbp)(B&) = &d.gb2; // { dg-line d_free_test2_gb2 }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test2_gb2 }
  // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test2_gb2 }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::gb2'} "" { target *-*-* } d_free_test2_gb2 }

  void (*fdp)(D&) = &d.fd2; // { dg-line d_free_test2_fd2 }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "" { target *-*-* } d_free_test2_fd2 }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::fd2'} "" { target *-*-* } d_free_test2_fd2 }
  void (*gdp)(D&) = &d.gd2; // { dg-line d_free_test2_gd2 }
  // { dg-error {ISO C\+\+ forbids taking the address of a bound member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test2_gd2 }
  // { dg-bogus {ISO C\+\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to explicit object member function} "PR113075" { xfail *-*-* } d_free_test2_gd2 }
  // { dg-note {a pointer to explicit object member function can only be formed with '&D::gd2'} "" { target *-*-* } d_free_test2_gd2 }
}

