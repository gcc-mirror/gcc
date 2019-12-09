// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct S {
  int a = 1;
  constexpr S() = default;
};

constexpr S s;

union U {
  int a = 1;
  constexpr U() = default;
};

constexpr U u;

struct S2 {
  int a;
  constexpr S2() = default;
};

constexpr S2 s2; // { dg-error "uninitialized .const s2." }

union U2 {
  int a;
  constexpr U2() = default;
};

constexpr U2 u2; // { dg-error "uninitialized .const u2." }

struct S3 {
  // FIXME if it's anonymous union, we don't give the error below
  union {
    int a;
  } u;
  constexpr S3() = default;
};

constexpr S3 s3; // { dg-error "uninitialized .const s3." }

struct S4 {
  // FIXME if it's anonymous union, we don't give the error below
  union {
    int n;
  } u;
  constexpr S4() = default;
};

constexpr S4 s4; // { dg-error "uninitialized .const s4." }

struct S5 {
  union {
    int n = 0;
  };
  // FIXME if it's anonymous union, we don't give the error below
  union {
    int m;
  } u;
  constexpr S5() = default;
};

constexpr S5 s5; // { dg-error "uninitialized .const s5.|not a constant expression" }
