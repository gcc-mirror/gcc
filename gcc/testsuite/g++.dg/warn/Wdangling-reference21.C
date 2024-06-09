// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }
// Reduced from config/aarch64/aarch64-early-ra.cc.

template <typename T> struct array_slice {
  using iterator = T *;
  iterator begin();
  iterator end();
  iterator m_base;
};

struct allocno_group_info { };

char recog_data_2;
int record_constraints_op;
struct early_ra {
  using operand_mask = int;
  struct allocno_info {
    int is_earlyclobbered;
  };
  struct allocno_subgroup {
    array_slice<allocno_info> allocnos();
    allocno_group_info *group;
  };
  allocno_subgroup get_allocno_subgroup(int);
  void record_constraints();
};
void early_ra::record_constraints() {
  operand_mask earlyclobber_operands, matched_operands, unmatched_operands,
      matches_operands, op_mask = operand_mask();
  auto record_operand = [&](int, int) {
    operand_mask overlaps;
    matches_operands |= overlaps;
  };
  for (int opno = 0; recog_data_2; ++opno) {
    operand_mask op_mask = earlyclobber_operands |= op_mask;
    if (0)
      record_operand(1, 0);
  }
  if (op_mask || (matched_operands & unmatched_operands && 0))
    for (auto &allocno : get_allocno_subgroup(record_constraints_op).allocnos())
      allocno.is_earlyclobbered = true;
}

