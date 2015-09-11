// { dg-options "-Wabi-tag" }

struct __attribute ((abi_tag ("X"))) A { };

struct B			// { dg-warning "ABI tag" }
{
  virtual void f(A);		// { dg-message "declared here" }
};
