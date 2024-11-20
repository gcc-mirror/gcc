// { dg-options "" }

void sc_a () [[arm::streaming_compatible]];
void sc_a (); // { dg-error "conflicting types" }

void sc_b ();
void sc_b () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

void sc_c () [[arm::streaming_compatible]];
void sc_c () {} // { dg-error "conflicting types" }

void sc_d ();
void sc_d () [[arm::streaming_compatible]] {} // { dg-error "conflicting types" }

void sc_e () [[arm::streaming_compatible]] {}
void sc_e (); // { dg-error "conflicting types" }

void sc_f () {}
void sc_f () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

extern void (*sc_g) ();
extern void (*sc_g) () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

extern void (*sc_h) () [[arm::streaming_compatible]];
extern void (*sc_h) (); // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void s_a () [[arm::streaming]];
void s_a (); // { dg-error "conflicting types" }

void s_b ();
void s_b () [[arm::streaming]]; // { dg-error "conflicting types" }

void s_c () [[arm::streaming]];
void s_c () {} // { dg-error "conflicting types" }

void s_d ();
void s_d () [[arm::streaming]] {} // { dg-error "conflicting types" }

void s_e () [[arm::streaming]] {}
void s_e (); // { dg-error "conflicting types" }

void s_f () {}
void s_f () [[arm::streaming]]; // { dg-error "conflicting types" }

extern void (*s_g) ();
extern void (*s_g) () [[arm::streaming]]; // { dg-error "conflicting types" }

extern void (*s_h) () [[arm::streaming]];
extern void (*s_h) (); // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void mixed_a () [[arm::streaming]];
void mixed_a () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

void mixed_b () [[arm::streaming_compatible]];
void mixed_b () [[arm::streaming]]; // { dg-error "conflicting types" }

void mixed_c () [[arm::streaming]];
void mixed_c () [[arm::streaming_compatible]] {} // { dg-error "conflicting types" }

void mixed_d () [[arm::streaming_compatible]];
void mixed_d () [[arm::streaming]] {} // { dg-error "conflicting types" }

void mixed_e () [[arm::streaming]] {}
void mixed_e () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

void mixed_f () [[arm::streaming_compatible]] {}
void mixed_f () [[arm::streaming]]; // { dg-error "conflicting types" }

extern void (*mixed_g) () [[arm::streaming_compatible]];
extern void (*mixed_g) () [[arm::streaming]]; // { dg-error "conflicting types" }

extern void (*mixed_h) () [[arm::streaming]];
extern void (*mixed_h) () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

//----------------------------------------------------------------------------

void contradiction_1 () [[arm::streaming, arm::streaming_compatible]]; // { dg-warning "conflicts with attribute" }
void contradiction_2 () [[arm::streaming_compatible, arm::streaming]]; // { dg-warning "conflicts with attribute" }

int [[arm::streaming_compatible]] int_attr; // { dg-warning "only applies to function types" }
void [[arm::streaming_compatible]] ret_attr (); // { dg-warning "only applies to function types" }
void *[[arm::streaming]] ptr_attr; // { dg-warning "only applies to function types" }

typedef void s_callback () [[arm::streaming]];
typedef void sc_callback () [[arm::streaming_compatible]];

typedef void contradiction_callback_1 () [[arm::streaming, arm::streaming_compatible]]; // { dg-warning "conflicts with attribute" }
typedef void contradiction_callback_2 () [[arm::streaming_compatible, arm::streaming]]; // { dg-warning "conflicts with attribute" }

void (*contradiction_callback_ptr_1) () [[arm::streaming, arm::streaming_compatible]]; // { dg-warning "conflicts with attribute" }
void (*contradiction_callback_ptr_2) () [[arm::streaming_compatible, arm::streaming]]; // { dg-warning "conflicts with attribute" }

struct s {
  void (*contradiction_callback_ptr_1) () [[arm::streaming, arm::streaming_compatible]]; // { dg-warning "conflicts with attribute" }
  void (*contradiction_callback_ptr_2) () [[arm::streaming_compatible, arm::streaming]]; // { dg-warning "conflicts with attribute" }
};

//----------------------------------------------------------------------------

void keyword_ok_1 () __arm_streaming;
void keyword_ok_1 () __arm_streaming;

void keyword_ok_2 () __arm_streaming;
void keyword_ok_2 () [[arm::streaming]];

void keyword_ok_3 () [[arm::streaming]];
void keyword_ok_3 () __arm_streaming;

void keyword_ok_4 () __arm_streaming [[arm::streaming]];

void keyword_ok_5 () __arm_streaming_compatible;
void keyword_ok_5 () [[arm::streaming_compatible]];

//----------------------------------------------------------------------------

void keyword_contradiction_1 () __arm_streaming;
void keyword_contradiction_1 (); // { dg-error "conflicting types" }

void keyword_contradiction_2 ();
void keyword_contradiction_2 () __arm_streaming; // { dg-error "conflicting types" }

void keyword_contradiction_3 () __arm_streaming;
void keyword_contradiction_3 () [[arm::streaming_compatible]]; // { dg-error "conflicting types" }

void keyword_contradiction_4 () [[arm::streaming_compatible]];
void keyword_contradiction_4 () __arm_streaming; // { dg-error "conflicting types" }
