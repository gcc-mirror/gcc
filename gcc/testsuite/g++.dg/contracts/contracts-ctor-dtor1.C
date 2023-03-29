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
// { dg-output "contract violation in function S::S at .*.C:11 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::S at .*.C:11 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SInline::SInline at .*.C:20 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SInline::SInline at .*.C:20 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate0::SDelegate0 at .*.C:26 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate0::SDelegate0 at .*.C:26 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate1::SDelegate1 at .*.C:34 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate1::SDelegate1 at .*.C:34 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::SDelegate2 at .*.C:41 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::SDelegate2 at .*.C:40 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::SDelegate2 at .*.C:40 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::SDelegate2 at .*.C:41 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::SDelegate3 at .*.C:48 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::SDelegate3 at .*.C:47 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::SDelegate3 at .*.C:47 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::SDelegate3 at .*.C:48 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1<int>::S1 at .*.C:55 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1<int>::S1 at .*.C:55 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1<double>::S1 at .*.C:55 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1<double>::S1 at .*.C:55 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S2::S2<int> at .*.C:61 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S2::S2<int> at .*.C:61 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S2::S2<double> at .*.C:61 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S2::S2<double> at .*.C:61 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<int>::S3<int> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<int>::S3<int> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<int>::S3<double> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<int>::S3<double> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<double>::S3<int> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<double>::S3<int> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<double>::S3<double> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S3<double>::S3<double> at .*.C:68 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::~SDelegate3 at .*.C:49 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate3::~SDelegate3 at .*.C:49 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::~SDelegate2 at .*.C:42 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate2::~SDelegate2 at .*.C:42 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate1::~SDelegate1 at .*.C:35 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate1::~SDelegate1 at .*.C:35 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate0::~SDelegate0 at .*.C:28 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SDelegate0::~SDelegate0 at .*.C:28 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SInline::~SInline at .*.C:21 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function SInline::~SInline at .*.C:21 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::~S at .*.C:12 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::~S at .*.C:12 .*(\n|\r\n|\r)" }

// test1
// { dg-output "contract violation in function G0::G0 at .*.C:73 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function G1::G1 at .*.C:80 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function G1::~G1 at .*.C:81 .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function G0::~G0 at .*.C:74 .*(\n|\r\n|\r)" }

