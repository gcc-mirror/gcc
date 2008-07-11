// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/20152

struct KrSelectionMode {  virtual void init() = 0; }; // { dg-error "24: error: previous definition of 'struct KrSelectionMode'" }
struct KrKDESelectionMode : public KrSelectionMode { void init() { } }; // { dg-error "52: error: previous definition of 'struct KrKDESelectionMode'" }
struct KrSelectionMode {  virtual void init() = 0; }; // { dg-error "8: error: redefinition of 'struct KrSelectionMode'" }
struct KrKDESelectionMode : public KrSelectionMode { void init() { } }; // { dg-error "8: error: redefinition of 'struct KrKDESelectionMode'" }
KrKDESelectionMode krKDESelectionMode;
