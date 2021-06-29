// Tests to ensure that contracts are properly emitted for constructors,
// destructors, and their intersection with templates.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

bool pre_{false}, post_{false};
bool delegate_{false};

struct S
{
  S() [[ pre: pre_ ]] [[ post: post_ ]];
  ~S() [[ pre: pre_ ]] [[ post: post_ ]];
};

S::S() { return; }
S::~S() { return; }

struct SInline
{
  SInline() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
  ~SInline() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
};

struct SDelegate0
{
  SDelegate0(int) [[ pre: pre_ ]] [[ post: post_ ]] { return; }
  SDelegate0() : SDelegate0(0) { return; }
  ~SDelegate0() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
};

struct SDelegate1
{
  SDelegate1(int) { return; }
  SDelegate1() [[ pre: pre_ ]] [[ post: post_ ]] : SDelegate1(0) { return; }
  ~SDelegate1() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
};

struct SDelegate2
{
  SDelegate2(int) [[ pre: pre_ ]] [[ post: post_ ]] { return; }
  SDelegate2() [[ pre: pre_ && delegate_ ]] [[ post: post_ && delegate_ ]] : SDelegate2(0) { return; }
  ~SDelegate2() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
};

struct SDelegate3
{
  SDelegate3(int) [[ pre: pre_ ]] [[ post: post_ ]] { return; }
  SDelegate3() [[ pre: pre_ && delegate_ ]] [[ post: post_ && delegate_ ]] : SDelegate3(0) { return; }
  ~SDelegate3() [[ pre: pre_ ]] [[ post: post_ ]] { return; }
};

template<typename T>
struct S1
{
  S1() [[ pre: pre_ ]] [[ post: post_ ]] { }
};

struct S2
{
  template<typename T>
  S2(T) [[ pre: pre_ ]] [[ post: post_ ]] { }
};

template<typename T>
struct S3
{
  template<typename S>
  S3(S) [[ pre: pre_ ]] [[ post: post_ ]] { }
};

struct G0
{
  G0() [[ post: x > 0 ]] {}
  ~G0() [[ pre: x > 0 ]] {}
  int x{-1};
};

struct G1
{
  G1() [[ post: this->x > 0 ]] {}
  ~G1() [[ pre: this->x > 0 ]] {}
  int x{-1};
};

int x{-1};

struct G2
{
  G2() [[ pre: ::x > 0 ]] {}
  ~G2() [[ post: ::x > 0 ]] {}
  int x{1};
};

void test0()
{
  S s;
  SInline si;
  SDelegate0 sd0;
  SDelegate1 sd1;
  SDelegate2 sd2;
  SDelegate3 sd3;
  S1<int> s1_i;
  S1<double> s1_d;
  S2 s2_i{1};
  S2 s2_d{.1};
  S3<int> s3_i_i{1};
  S3<int> s3_i_d{.1};
  S3<double> s3_d_i{1};
  S3<double> s3_d_d{.1};
}

void test1()
{
  G0 g0;
  G1 g1;
  G2 g2;
}

int main(int, char**)
{
  test0();
  test1();
  return 0;
};

// test0
// { dg-output "default std::handle_contract_violation called: .*.C 11 S::S .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 11 S::S .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 SInline::SInline .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 SInline::SInline .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 26 SDelegate0::SDelegate0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 26 SDelegate0::SDelegate0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 34 SDelegate1::SDelegate1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 34 SDelegate1::SDelegate1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 41 SDelegate2::SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 40 SDelegate2::SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 40 SDelegate2::SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 41 SDelegate2::SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 48 SDelegate3::SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 47 SDelegate3::SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 47 SDelegate3::SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 48 SDelegate3::SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 S1<int>::S1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 S1<int>::S1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 S1<double>::S1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 S1<double>::S1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 S2::S2<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 S2::S2<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 S2::S2<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 S2::S2<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<int>::S3<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<int>::S3<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<int>::S3<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<int>::S3<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<double>::S3<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<double>::S3<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<double>::S3<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 S3<double>::S3<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 49 SDelegate3::~SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 49 SDelegate3::~SDelegate3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 42 SDelegate2::~SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 42 SDelegate2::~SDelegate2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 35 SDelegate1::~SDelegate1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 35 SDelegate1::~SDelegate1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 28 SDelegate0::~SDelegate0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 28 SDelegate0::~SDelegate0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 21 SInline::~SInline .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 21 SInline::~SInline .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 S::~S .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 S::~S .*(\n|\r\n|\r)*" }

// test1
// { dg-output "default std::handle_contract_violation called: .*.C 73 G0::G0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 80 G1::G1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 81 G1::~G1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 74 G0::~G0 .*(\n|\r\n|\r)*" }

