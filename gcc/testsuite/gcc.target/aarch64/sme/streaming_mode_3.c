// { dg-options "" }

#pragma GCC target "+nosme"

void sc_a () [[arm::streaming_compatible]] {}
void s_a () [[arm::streaming]] {} // { dg-error "streaming functions require the ISA extension 'sme'" }
void ns_a () {}

void sc_b () [[arm::streaming_compatible]] {}
void ns_b () {}
void s_b () [[arm::streaming]] {} // { dg-error "streaming functions require the ISA extension 'sme'" }

void sc_c () [[arm::streaming_compatible]] {}
void sc_d () [[arm::streaming_compatible]] {}

void s_c () [[arm::streaming]] {} // { dg-error "streaming functions require the ISA extension 'sme'" }
void s_d () [[arm::streaming]] {} // { dg-error "streaming functions require the ISA extension 'sme'" }

void ns_c () {}
void ns_d () {}

void sc_e () [[arm::streaming_compatible]];
void s_e () [[arm::streaming]];
void ns_e ();

#pragma GCC target "+sme"

void sc_f () [[arm::streaming_compatible]] {}
void s_f () [[arm::streaming]] {}
void ns_f () {}

void sc_g () [[arm::streaming_compatible]] {}
void ns_g () {}
void s_g () [[arm::streaming]] {}

void sc_h () [[arm::streaming_compatible]] {}
void sc_i () [[arm::streaming_compatible]] {}

void s_h () [[arm::streaming]] {}
void s_i () [[arm::streaming]] {}

void ns_h () {}
void ns_i () {}

void sc_j () [[arm::streaming_compatible]];
void s_j () [[arm::streaming]];
void ns_j ();

#pragma GCC target "+sme"

void sc_k () [[arm::streaming_compatible]] {}

#pragma GCC target "+nosme"
#pragma GCC target "+sme"

void s_k () [[arm::streaming]] {}

#pragma GCC target "+nosme"
#pragma GCC target "+sme"

void ns_k () {}

#pragma GCC target "+nosme"
