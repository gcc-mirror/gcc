// { dg-options "" }

[[arm::new("za")]] void new_za_a ();
void new_za_a ();

void new_za_b ();
[[arm::new("za")]] void new_za_b ();

[[arm::new("za")]] void new_za_c ();
void new_za_c () {}

void new_za_d ();
[[arm::new("za")]] void new_za_d () {}

[[arm::new("za")]] void new_za_e () {}
void new_za_e ();

void new_za_f () {}
[[arm::new("za")]] void new_za_f (); // { dg-error "cannot apply attribute 'new' to 'new_za_f' after the function has been defined" }

//----------------------------------------------------------------------------

[[arm::new("za")]] void shared_a ();
void shared_a () [[arm::inout("za")]]; // { dg-error "conflicting types" }

void shared_b () [[arm::inout("za")]];
[[arm::new("za")]] void shared_b (); // { dg-error "conflicting types" }

[[arm::new("za")]] void shared_c ();
void shared_c () [[arm::in("za")]] {} // { dg-error "conflicting types" }

void shared_d () [[arm::in("za")]];
[[arm::new("za")]] void shared_d () {} // { dg-error "conflicting types" }

[[arm::new("za")]] void shared_e () {}
void shared_e () [[arm::out("za")]]; // { dg-error "conflicting types" }

void shared_f () [[arm::out("za")]] {}
[[arm::new("za")]] void shared_f (); // { dg-error "conflicting types" }

[[arm::new("za")]] void shared_g () {}
void shared_g () [[arm::preserves("za")]]; // { dg-error "conflicting types" }

void shared_h () [[arm::preserves("za")]] {}
[[arm::new("za")]] void shared_h (); // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

[[arm::new("za")]] void contradiction_1 () [[arm::inout("za")]]; // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }
void contradiction_2 [[arm::new("za")]] () [[arm::inout("za")]]; // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }
[[arm::new("za")]] void contradiction_3 () [[arm::preserves("za")]]; // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }
void contradiction_4 [[arm::new("za")]] () [[arm::preserves("za")]]; // { dg-error "cannot create a new 'za' scope since 'za' is shared with callers" }

int [[arm::new("za")]] int_attr; // { dg-warning "does not apply to types" }
[[arm::new("za")]] int int_var_attr; // { dg-error "applies only to function definitions" }
typedef void new_za_callback () [[arm::new("za")]]; // { dg-warning "does not apply to types" }
[[arm::new("za")]] void (*new_za_var_callback) (); // { dg-error "applies only to function definitions" }

//----------------------------------------------------------------------------

[[arm::new("za")]] void complementary_1 () [[arm::streaming]] {}
void complementary_2 [[arm::new("za")]] () [[arm::streaming]] {}
[[arm::new("za")]] void complementary_3 () [[arm::streaming_compatible]] {}
void complementary_4 [[arm::new("za")]] () [[arm::streaming_compatible]] {}

//----------------------------------------------------------------------------

#pragma GCC target "+nosme"

[[arm::new("za")]] void bereft_1 ();
[[arm::new("za")]] void bereft_2 () {} // { dg-error "functions with SME state require the ISA extension 'sme'" }
void bereft_3 () [[arm::inout("za")]];
void bereft_4 () [[arm::inout("za")]] {} // { dg-error "functions with SME state require the ISA extension 'sme'" }
