// { dg-options "-mgeneral-regs-only" }

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
