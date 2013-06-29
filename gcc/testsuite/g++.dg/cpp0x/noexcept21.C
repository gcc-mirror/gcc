// PR c++/57645
// { dg-do compile { target c++11 } }

struct Thrower
{
  ~Thrower() noexcept(false) { throw 1; }
};

struct ExplicitA
{
  ~ExplicitA() {}

  Thrower t;
};

struct ExplicitB
{
  ~ExplicitB();

  Thrower t;
};

ExplicitB::~ExplicitB() {}

struct ExplicitC
{
  ~ExplicitC() = default;

  Thrower t;
};

struct ExplicitD
{
  ~ExplicitD();

  Thrower t;
};

ExplicitD::~ExplicitD() = default;

struct NoThrower
{
  ~NoThrower() noexcept(true) {}
};

struct ExplicitE
{
  ~ExplicitE() {}

  NoThrower t;
};

struct ExplicitF
{
  ~ExplicitF();

  NoThrower t;
};

ExplicitF::~ExplicitF() {}

struct ExplicitG
{
  ~ExplicitG() = default;

  NoThrower t;
};

struct ExplicitH
{
  ~ExplicitH();

  NoThrower t;
};

ExplicitH::~ExplicitH() = default;

#define SA(X) static_assert(X, #X)

SA( !noexcept(ExplicitA()) );
SA( !noexcept(ExplicitB()) );
SA( !noexcept(ExplicitC()) );
SA( !noexcept(ExplicitD()) );
SA( noexcept(ExplicitE()) );
SA( noexcept(ExplicitF()) );
SA( noexcept(ExplicitG()) );
SA( noexcept(ExplicitH()) );
