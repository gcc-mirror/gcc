// { dg-do compile }

struct HARD_REG_SET {
  HARD_REG_SET operator~() const {
    HARD_REG_SET res;
    for (unsigned int i = 0; i < (sizeof(elts) / sizeof((elts)[0])); ++i)
      res.elts[i] = ~elts[i];
    return res;
  }
  HARD_REG_SET operator&(const HARD_REG_SET &other) const {
    HARD_REG_SET res;
    for (unsigned int i = 0; i < (sizeof(elts) / sizeof((elts)[0])); ++i)
      res.elts[i] = elts[i] & other.elts[i];
    return res;
  }
  unsigned long elts[4];
};
typedef const HARD_REG_SET &const_hard_reg_set;
inline bool hard_reg_set_subset_p(const_hard_reg_set x, const_hard_reg_set y) {
  unsigned long bad = 0;
  for (unsigned int i = 0; i < (sizeof(x.elts) / sizeof((x.elts)[0])); ++i)
    bad |= (x.elts[i] & ~y.elts[i]);
  return bad == 0;
}
inline bool hard_reg_set_empty_p(const_hard_reg_set x) {
  unsigned long bad = 0;
  for (unsigned int i = 0; i < (sizeof(x.elts) / sizeof((x.elts)[0])); ++i)
    bad |= x.elts[i];
  return bad == 0;
}
extern HARD_REG_SET rr[2];
extern int t[2];
extern HARD_REG_SET nn;
static HARD_REG_SET mm;
void setup_reg_class_relations(void) {
  HARD_REG_SET intersection_set, union_set, temp_set2;
  for (int cl2 = 0; cl2 < 2; cl2++) {
    temp_set2 = rr[cl2] & ~nn;
    if (hard_reg_set_empty_p(mm) && hard_reg_set_empty_p(temp_set2)) {
      mm = rr[0] & nn;
      if (hard_reg_set_subset_p(mm, intersection_set))
        if (!hard_reg_set_subset_p(mm, temp_set2) ||
            hard_reg_set_subset_p(rr[0], rr[t[cl2]]))
          t[cl2] = 0;
    }
  }
}
