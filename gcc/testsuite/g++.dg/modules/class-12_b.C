// { dg-additional-options "-fmodules" }
// { dg-module-cmi !X }

export module X;
import M;

Global::Global() = default; // { dg-error "in module .X. conflicts with import" }
Global::Global(int) {}	    // { dg-error "in module .X. conflicts with import" }
void Global::foo() {}	    // { dg-error "in module .X. conflicts with import" }
int Global::x;		    // { dg-error "in module .X. conflicts with import" }
struct Global::S {};	    // { dg-error "in module .X. conflicts with import" }

template <typename> void Global::a() {}	  // { dg-error "in module .X. conflicts with import" }
template <typename> int Global::r {};	  // { dg-error "in module .X. conflicts with import" }
template <typename> struct Global::X {};  // { dg-error "in module .X. conflicts with import" }

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

Module::Module() = default; // { dg-error "in module .X. conflicts with import" }
Module::Module(int) {}	    // { dg-error "in module .X. conflicts with import" }
void Module::foo() {}	    // { dg-error "in module .X. conflicts with import" }
int Module::x;		    // { dg-error "in module .X. conflicts with import" }
struct Module::S {};	    // { dg-error "in module .X. conflicts with import" }

template <typename> void Module::a() {}	  // { dg-error "in module .X. conflicts with import" }
template <typename> int Module::r {};	  // { dg-error "in module .X. conflicts with import" }
template <typename> struct Module::X {};  // { dg-error "in module .X. conflicts with import" }

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
