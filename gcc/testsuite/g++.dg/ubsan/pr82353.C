/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -std=c++11 -fsanitize=undefined -fno-sanitize-recover=undefined -w -fdump-rtl-reload" } */

extern unsigned long tf_2_var_1, tf_2_var_21;
extern bool tf_2_var_2, tf_2_var_24, tf_2_var_6, tf_2_var_5;
extern unsigned char tf_2_var_16, tf_2_var_31;
extern short tf_2_var_69;
extern unsigned tf_2_var_233;
struct tf_2_struct_1 {
  short member_1_0 : 27;
  long member_1_1 : 10;
};
struct a {
  int member_2_0 : 5;
};
struct tf_2_struct_3 {
  static tf_2_struct_1 member_3_0;
};
struct tf_2_struct_4 {
  static unsigned member_4_0;
  a member_4_1;
};
struct tf_2_struct_5 {
  tf_2_struct_1 member_5_2;
  tf_2_struct_4 member_5_4;
};
struct tf_2_struct_6 {
  tf_2_struct_5 member_6_2;
  short member_6_4;
} extern tf_2_struct_obj_2;
extern tf_2_struct_3 tf_2_struct_obj_8;
tf_2_struct_1 a;
tf_2_struct_5 b;
tf_2_struct_1 tf_2_struct_3::member_3_0;
unsigned tf_2_struct_4::member_4_0;
void tf_2_init() {
  a.member_1_1 = tf_2_struct_obj_2.member_6_2.member_5_2.member_1_1 = 5;
}
void tf_2_foo() {
  int c = tf_2_struct_obj_2.member_6_2.member_5_4.member_4_1.member_2_0 -
          -~tf_2_struct_obj_2.member_6_4 * char(90284000534361);
  tf_2_struct_obj_8.member_3_0.member_1_0 =
      tf_2_var_24 >
      tf_2_var_21 * a.member_1_0 * tf_2_var_2 - tf_2_var_5 % a.member_1_1;
  if ((~(tf_2_var_31 * tf_2_var_6) &&
       -~tf_2_struct_obj_2.member_6_4 * 90284000534361) %
      ~tf_2_var_31 * tf_2_var_6)
    b.member_5_2.member_1_0 << tf_2_var_16 << tf_2_var_1;
  tf_2_var_233 = -~tf_2_struct_obj_2.member_6_4 * char(90284000534361);
  int d(tf_2_struct_obj_2.member_6_4);
  if (b.member_5_2.member_1_0)
    b.member_5_2.member_1_1 = c;
  bool e(~-~tf_2_struct_obj_2.member_6_4);
  a.member_1_1 % e;
  if (tf_2_var_5 / tf_2_struct_obj_2.member_6_2.member_5_2.member_1_1)
    b.member_5_4.member_4_0 = tf_2_var_21 * a.member_1_0 * tf_2_var_2;
  tf_2_var_69 = tf_2_var_6;
}

/* { dg-final { scan-rtl-dump-not "Inserting rematerialization insn" "reload" } } */
