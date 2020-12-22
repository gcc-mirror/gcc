// { dg-additional-options {-fmodules-ts -fdump-lang-module} }
module foo;

// completes class A from interface
class A
{
};

void bill ()
{
  A a;
}

// redeclaration of class B{} from interface
class B;

void bob ()
{
  B b;
}

// { dg-final { scan-lang-dump {Lazily binding '::A'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::B'@'foo' section:} module } }
