// { dg-options "" }

#pragma GCC target "+sme2"

void share_za_zt0_a() __arm_inout("za", "zt0");
void share_za_zt0_b() __arm_inout("za", "zt0");

void share_za_preserve_zt0() __arm_inout("za") __arm_preserves("zt0");
void share_zt0_preserve_za() __arm_inout("zt0") __arm_preserves("za");

__arm_new("za", "zt0") void new_za_zt0_a() {
  share_za_zt0_a();
  share_za_zt0_b();
}

__arm_new("zt0", "za") void new_za_zt0_b() {
  share_za_zt0_a();
  share_za_zt0_b();
}

__arm_new("zt0") void new_za_zt0_c();
__arm_new("za") void new_za_zt0_c() {
  share_za_zt0_a();
  share_za_zt0_b();
}

__arm_new("za") void new_za_zt0_d();
__arm_new("zt0") void new_za_zt0_d() {
  share_za_zt0_a();
  share_za_zt0_b();
}

__arm_new("zt0", "za") void new_za_zt0_e();
void new_za_zt0_e() {
  share_za_zt0_a();
  share_za_zt0_b();
}

__arm_new("zt0") void new_zt0_a() {
  share_za_zt0_a(); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
}

__arm_new("zt0") void new_zt0_b();
void new_zt0_b() {
  share_za_preserve_zt0(); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
}

__arm_new("zt0") void new_zt0_c();
void new_zt0_c() {
  share_zt0_preserve_za();
}

__arm_new("za") void new_za_a() {
  share_za_zt0_a(); // { dg-error {call to a function that shares 'zt0' state from a function that has no 'zt0' state} }
}

__arm_new("za") void new_za_b();
void new_za_b() {
  share_za_preserve_zt0();
}

__arm_new("za") void new_za_c();
void new_za_c() {
  share_zt0_preserve_za(); // { dg-error {call to a function that shares 'zt0' state from a function that has no 'zt0' state} }
}
