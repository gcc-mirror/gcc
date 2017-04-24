// { dg-options "-fdiagnostics-show-caret" }
// { dg-do compile { target c++11 } }

struct A
{
  virtual void foo() const;
};

struct B final final : A  /* { dg-error "duplicate virt-specifier" }
  { dg-begin-multiline-output "" }
 struct B final final : A
                ^~~~~
                -----
  { dg-end-multiline-output "" } */
{
  virtual void foo() const override final override;  /* { dg-error "duplicate virt-specifier" }
  { dg-begin-multiline-output "" }
   virtual void foo() const override final override;
                                           ^~~~~~~~
                                           --------
  { dg-end-multiline-output "" } */
};

thread_local thread_local int i = 0;  /* { dg-error "duplicate" }
  { dg-begin-multiline-output "" }
 thread_local thread_local int i = 0;
              ^~~~~~~~~~~~
              ------------
  { dg-end-multiline-output "" } */
