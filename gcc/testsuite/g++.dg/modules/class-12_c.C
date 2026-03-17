// { dg-additional-options "-fmodules" }

module M:part;
import M;

Global::Global() = default; // { dg-error "in module .M. conflicts with import" }
Global::Global(int) {}	    // { dg-error "in module .M. conflicts with import" }
void Global::foo() {}	    // { dg-error "in module .M. conflicts with import" }
int Global::x;		    // { dg-error "in module .M. conflicts with import" }
struct Global::S {};	    // { dg-error "in module .M. conflicts with import" }

template <typename> void Global::a() {}	  // { dg-error "in module .M. conflicts with import" }
template <typename> int Global::r {};	  // { dg-error "in module .M. conflicts with import" }
template <typename> struct Global::X {};  // { dg-error "in module .M. conflicts with import" }

extern "C++" {
  Global::Global(const Global &) = default;
  Global::Global(double) {}
  void Global::bar() {}
  int Global::y;
  struct Global::T {};

  template <typename> void Global::b() {}
  template <typename> int Global::s {};
  template <typename> struct Global::Y {};
}

Module::Module() = default;
Module::Module(int) {}
void Module::foo() {}
int Module::x;
struct Module::S {};

template <typename> void Module::a() {}
template <typename> int Module::r {};
template <typename> struct Module::X{};

extern "C++" {
  Module::Module(const Module &) = default; // { dg-error "in global module conflicts with import" }
  Module::Module(double) {}		    // { dg-error "in global module conflicts with import" }
  void Module::bar() {}			    // { dg-error "in global module conflicts with import" }
  int Module::y;			    // { dg-error "in global module conflicts with import" }
  struct Module::T {};			    // { dg-error "in global module conflicts with import" }

  template <typename> void Module::b() {}   // { dg-error "in global module conflicts with import" }
  template <typename> int Module::s {};	    // { dg-error "in global module conflicts with import" }
  template <typename> struct Module::Y {};  // { dg-error "in global module conflicts with import" }
}
