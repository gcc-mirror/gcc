// PR c++/20152

struct KrSelectionMode {  virtual void init() = 0; }; // { dg-error "previous definition" }
struct KrKDESelectionMode : public KrSelectionMode { void init() { } }; // { dg-error "previous definition" }
struct KrSelectionMode {  virtual void init() = 0; }; // { dg-error "" }
struct KrKDESelectionMode : public KrSelectionMode { void init() { } }; // { dg-error "" }
KrKDESelectionMode krKDESelectionMode;
