// { dg-options "" }

void invalid_a() __arm_inout("za");
void invalid_a() __arm_inout("za", "zt0"); // { dg-error {conflicting types} }

void invalid_b() __arm_inout("za", "zt0");
void invalid_b() __arm_inout("zt0"); // { dg-error {conflicting types} }

void invalid_c() __arm_in("zt0") __arm_inout("za");
void invalid_c() __arm_inout("zt0", "za"); // { dg-error {conflicting types} }

void invalid_d() __arm_inout("zt0");
void invalid_d() __arm_out("zt0"); // { dg-error {conflicting types} }

void invalid_e() __arm_in("zt0");
void invalid_e() __arm_out("zt0"); // { dg-error {conflicting types} }

void invalid_f() __arm_in("zt0");
void invalid_f() __arm_preserves("zt0"); // { dg-error {conflicting types} }

void valid_a() __arm_inout("zt0") __arm_inout("za");
void valid_a() __arm_inout("zt0", "za");

void valid_b() __arm_inout("za") __arm_inout("zt0");
void valid_b() __arm_inout("zt0") __arm_inout("za");

void valid_c() __arm_inout("za", "zt0");
void valid_c() __arm_inout("zt0", "za");

void valid_d() __arm_inout("zt0", "za");
void valid_d() __arm_inout("za", "zt0");
