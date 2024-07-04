// { dg-options "" }

void shared_a () [[arm::inout("za")]];
void shared_a (); // { dg-error "conflicting types" }

void shared_b ();
void shared_b () [[arm::inout("za")]]; // { dg-error "conflicting types" }

void shared_c () [[arm::inout("za")]];
void shared_c () {} // Inherits attribute from declaration (confusingly).

void shared_d ();
void shared_d () [[arm::inout("za")]] {} // { dg-error "conflicting types" }

void shared_e () [[arm::inout("za")]] {}
void shared_e (); // { dg-error "conflicting types" }

void shared_f () {}
void shared_f () [[arm::inout("za")]]; // { dg-error "conflicting types" }

extern void (*shared_g) ();
extern void (*shared_g) () [[arm::inout("za")]]; // { dg-error "conflicting types" }

extern void (*shared_h) () [[arm::inout("za")]];
extern void (*shared_h) (); // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void preserved_a () [[arm::preserves("za")]];
void preserved_a (); // { dg-error "conflicting types" }

void preserved_b ();
void preserved_b () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

void preserved_c () [[arm::preserves("za")]];
void preserved_c () {} // Inherits attribute from declaration (confusingly).

void preserved_d ();
void preserved_d () [[arm::preserves("za")]] {} // { dg-error "conflicting types" }

void preserved_e () [[arm::preserves("za")]] {}
void preserved_e (); // { dg-error "conflicting types" }

void preserved_f () {}
void preserved_f () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

extern void (*preserved_g) ();
extern void (*preserved_g) () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

extern void (*preserved_h) () [[arm::preserves("za")]];
extern void (*preserved_h) (); // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void replicated_1 () [[arm::in("za", "za"), arm::in("za")]];
void replicated_2 () [[arm::out("za", "za"), arm::out("za")]];
void replicated_3 () [[arm::inout("za", "za"), arm::inout("za")]];
void replicated_4 () [[arm::preserves("za", "za"), arm::preserves("za")]];

//----------------------------------------------------------------------------

void invalid_1 () [[arm::in]]; // { dg-error "wrong number of arguments" }
void invalid_2 () [[arm::in()]]; // { dg-error "parentheses must be omitted" }
  // { dg-error "wrong number of arguments" "" { target *-*-* } .-1 }
void invalid_3 () [[arm::in("")]]; // { dg-error "unrecognized state string ''" }
void invalid_4 () [[arm::in("foo")]]; // { dg-error "unrecognized state string 'foo'" }
void invalid_5 () [[arm::in(42)]]; // { dg-error "the arguments to 'in' must be constant strings" }
void invalid_6 () [[arm::in(*(int *)0 ? "za" : "za")]]; // { dg-error "the arguments to 'in' must be constant strings" }

//----------------------------------------------------------------------------

void mixed_a () [[arm::preserves("za")]];
void mixed_a () [[arm::inout("za")]]; // { dg-error "conflicting types" }

void mixed_b () [[arm::inout("za")]];
void mixed_b () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

void mixed_c () [[arm::preserves("za")]];
void mixed_c () [[arm::in("za")]] {} // { dg-error "conflicting types" }

void mixed_d () [[arm::inout("za")]];
void mixed_d () [[arm::in("za")]] {} // { dg-error "conflicting types" }

void mixed_e () [[arm::out("za")]] {}
void mixed_e () [[arm::in("za")]]; // { dg-error "conflicting types" }

void mixed_f () [[arm::inout("za")]] {}
void mixed_f () [[arm::out("za")]]; // { dg-error "conflicting types" }

extern void (*mixed_g) () [[arm::in("za")]];
extern void (*mixed_g) () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

extern void (*mixed_h) () [[arm::preserves("za")]];
extern void (*mixed_h) () [[arm::out("za")]]; // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void contradiction_1 () [[arm::preserves("za"), arm::inout("za")]]; // { dg-error "inconsistent attributes for state 'za'" }
void contradiction_2 () [[arm::inout("za"), arm::preserves("za")]]; // { dg-error "inconsistent attributes for state 'za'" }

int [[arm::inout("za")]] int_attr; // { dg-warning "only applies to function types" }
void *[[arm::preserves("za")]] ptr_attr; // { dg-warning "only applies to function types" }

typedef void preserved_callback () [[arm::preserves("za")]];
typedef void shared_callback () [[arm::inout("za")]];

void (*preserved_callback_ptr) () [[arm::preserves("za")]];
void (*shared_callback_ptr) () [[arm::inout("za")]];

typedef void contradiction_callback_1 () [[arm::preserves("za"), arm::inout("za")]]; // { dg-error "inconsistent attributes for state 'za'" }
typedef void contradiction_callback_2 () [[arm::inout("za"), arm::preserves("za")]]; // { dg-error "inconsistent attributes for state 'za'" }

void (*contradiction_callback_ptr_1) () [[arm::preserves("za"), arm::inout("za")]]; // { dg-error "inconsistent attributes for state 'za'" }
void (*contradiction_callback_ptr_2) () [[arm::inout("za"), arm::preserves("za")]]; // { dg-error "inconsistent attributes for state 'za'" }

struct s {
  void (*contradiction_callback_ptr_1) () [[arm::preserves("za"), arm::inout("za")]]; // { dg-error "inconsistent attributes for state 'za'" }
  void (*contradiction_callback_ptr_2) () [[arm::inout("za"), arm::preserves("za")]]; // { dg-error "inconsistent attributes for state 'za'" }
};

//----------------------------------------------------------------------------

void keyword_ok_1 () __arm_inout("za");
void keyword_ok_1 () __arm_inout("za");

void keyword_ok_2 () __arm_in("za");
void keyword_ok_2 () [[arm::in("za")]];

void keyword_ok_3 () [[arm::out("za")]];
void keyword_ok_3 () __arm_out("za");

void keyword_ok_4 () __arm_inout("za") [[arm::inout("za")]];

void keyword_ok_5 () __arm_preserves("za");
void keyword_ok_5 () [[arm::preserves("za")]];

__arm_new("za") void keyword_ok_6 () {}

//----------------------------------------------------------------------------

void keyword_conflict_1 () __arm_inout("za");
void keyword_conflict_1 (); // { dg-error "conflicting types" }

void keyword_conflict_2 ();
void keyword_conflict_2 () __arm_inout("za"); // { dg-error "conflicting types" }

void keyword_conflict_3 () __arm_inout("za");
void keyword_conflict_3 () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

void keyword_conflict_4 () [[arm::preserves("za")]];
void keyword_conflict_4 () __arm_inout("za"); // { dg-error "conflicting types" }

__arm_new("za") void keyword_conflict_5 () __arm_inout("za") {} // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }
__arm_new("za") void keyword_conflict_6 () __arm_preserves("za") {} // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }
