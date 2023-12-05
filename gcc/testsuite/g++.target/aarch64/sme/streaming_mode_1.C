// { dg-options "" }

void sc_a () [[arm::streaming_compatible]];
void sc_a (); // { dg-error "ambiguating new declaration" "" { xfail *-*-* } }

void sc_b ();
void sc_b () [[arm::streaming_compatible]]; // { dg-error "ambiguating new declaration" }

void sc_c () [[arm::streaming_compatible]];
void sc_c () {} // Inherits attribute from declaration (confusingly).

void sc_d ();
void sc_d () [[arm::streaming_compatible]] {} // { dg-error "ambiguating new declaration" }

void sc_e () [[arm::streaming_compatible]] {}
void sc_e (); // { dg-error "ambiguating new declaration" "" { xfail *-*-* } }

void sc_f () {}
void sc_f () [[arm::streaming_compatible]]; // { dg-error "ambiguating new declaration" }

extern void (*sc_g) ();
extern void (*sc_g) () [[arm::streaming_compatible]]; // { dg-error "conflicting declaration" }

extern void (*sc_h) () [[arm::streaming_compatible]];
extern void (*sc_h) (); // { dg-error "conflicting declaration" }

//----------------------------------------------------------------------------

void s_a () [[arm::streaming]];
void s_a (); // { dg-error "ambiguating new declaration" "" { xfail *-*-* } }

void s_b ();
void s_b () [[arm::streaming]]; // { dg-error "ambiguating new declaration" }

void s_c () [[arm::streaming]];
void s_c () {} // Inherits attribute from declaration (confusingly).

void s_d ();
void s_d () [[arm::streaming]] {} // { dg-error "ambiguating new declaration" }

void s_e () [[arm::streaming]] {}
void s_e (); // { dg-error "ambiguating new declaration" "" { xfail *-*-* } }

void s_f () {}
void s_f () [[arm::streaming]]; // { dg-error "ambiguating new declaration" }

extern void (*s_g) ();
extern void (*s_g) () [[arm::streaming]]; // { dg-error "conflicting declaration" }

extern void (*s_h) () [[arm::streaming]];
extern void (*s_h) (); // { dg-error "conflicting declaration" }

//----------------------------------------------------------------------------

void mixed_a () [[arm::streaming]];
void mixed_a () [[arm::streaming_compatible]]; // { dg-error "ambiguating new declaration" }

void mixed_b () [[arm::streaming_compatible]];
void mixed_b () [[arm::streaming]]; // { dg-error "ambiguating new declaration" }

void mixed_c () [[arm::streaming]];
void mixed_c () [[arm::streaming_compatible]] {} // { dg-error "ambiguating new declaration" }

void mixed_d () [[arm::streaming_compatible]];
void mixed_d () [[arm::streaming]] {} // { dg-error "ambiguating new declaration" }

void mixed_e () [[arm::streaming]] {}
void mixed_e () [[arm::streaming_compatible]]; // { dg-error "ambiguating new declaration" }

void mixed_f () [[arm::streaming_compatible]] {}
void mixed_f () [[arm::streaming]]; // { dg-error "ambiguating new declaration" }

extern void (*mixed_g) () [[arm::streaming_compatible]];
extern void (*mixed_g) () [[arm::streaming]]; // { dg-error "conflicting declaration" }

extern void (*mixed_h) () [[arm::streaming]];
extern void (*mixed_h) () [[arm::streaming_compatible]]; // { dg-error "conflicting declaration" }

//----------------------------------------------------------------------------

void contradiction_1 () [[arm::streaming, arm::streaming_compatible]]; // { dg-warning "conflicts with attribute" }
void contradiction_2 () [[arm::streaming_compatible, arm::streaming]]; // { dg-warning "conflicts with attribute" }

int [[arm::streaming_compatible]] int_attr; // { dg-warning "attribute ignored" }
void [[arm::streaming_compatible]] ret_attr (); // { dg-warning "attribute ignored" }
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
void keyword_contradiction_1 (); // { dg-error "ambiguating new declaration" "" { xfail *-*-* } }

void keyword_contradiction_2 ();
void keyword_contradiction_2 () __arm_streaming; // { dg-error "ambiguating new declaration" }

void keyword_contradiction_3 () __arm_streaming;
void keyword_contradiction_3 () [[arm::streaming_compatible]]; // { dg-error "ambiguating new declaration" }

void keyword_contradiction_4 () [[arm::streaming_compatible]];
void keyword_contradiction_4 () __arm_streaming; // { dg-error "ambiguating new declaration" }

//----------------------------------------------------------------------------

struct s1
{
  virtual void f () [[arm::streaming]];
};

struct s2 : public s1
{
  void f () override; // { dg-error "conflicting type attributes" }
};
