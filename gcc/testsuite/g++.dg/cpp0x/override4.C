// { dg-options "-std=c++11" }
// { dg-prune-output "expected ';'" }
// { dg-prune-output "expected unqualified-id" }
// { dg-prune-output "declaration does not declare anything" }

struct B
{
  virtual auto f() -> void final;
  virtual auto g() -> void;
};

struct B2
{
  virtual auto f() -> void final {}
};

struct B3
{
  virtual auto f() -> final void; // { dg-error "expected type-specifier" }
};

struct B4
{
  virtual auto f() -> final void {} // { dg-error "expected type-specifier" }
};

struct D : B
{
  virtual auto g() -> void override;
};

struct D2 : B
{
  virtual auto g() -> void override {}
};

struct D3 : B
{
  virtual auto g() -> override void; // { dg-error "expected type-specifier" }
};

struct D4 : B
{
  virtual auto g() -> override void {} // { dg-error "expected type-specifier" }
};
