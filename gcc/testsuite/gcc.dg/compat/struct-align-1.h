/* Define several variants of a struct for which the alignment differs
   between powerpc64-linux and powerpc64-aix.  This might be interesting
   for other targets as well.  */

#define DESC_orig "original"
struct B1_orig {
  char c;
  double d;
};

struct A2_orig {
  double d;
};

struct B2_orig {
  char c;
  struct A2_orig a2;
};

struct A3_orig {
  double d;
  int i;
};

struct B3_orig {
  char c;
  struct A3_orig a3;
};

#ifndef SKIP_ATTRIBUTE
#define DESC_p_all "packed attribute for all"
struct B1_p_all {
  char c;
  double d;
} __attribute__ ((packed));

struct A2_p_all {
  double d;
} __attribute__ ((packed));

struct B2_p_all {
  char c;
  struct A2_p_all a2;
} __attribute__ ((packed));

struct A3_p_all {
  double d;
  int i;
} __attribute__ ((packed));

struct B3_p_all {
  char c;
  struct A3_p_all a3;
} __attribute__ ((packed));

#define DESC_p_inner "packed attribute for inner"
struct B1_p_inner {
  char c;
  double d;
};

struct A2_p_inner {
  double d;
} __attribute__ ((packed));

struct B2_p_inner {
  char c;
  struct A2_p_inner a2;
};

struct A3_p_inner {
  double d;
  int i;
} __attribute__ ((packed));

struct B3_p_inner {
  char c;
  struct A3_p_inner a3;
};

#define DESC_p_outer "packed attribute for outer"
struct B1_p_outer {
  char c;
  double d;
} __attribute__ ((packed));

struct A2_p_outer {
  double d;
};

struct B2_p_outer {
  char c;
  struct A2_p_outer a2;
} __attribute__ ((packed));

struct A3_p_outer {
  double d;
  int i;
};

struct B3_p_outer {
  char c;
  struct A3_p_outer a3;
} __attribute__ ((packed));

#define DESC_a_max "maximum useful struct alignment for all"
struct B1_a_max {
  char c;
  double d;
} __attribute__ ((aligned));

struct A2_a_max {
  double d;
} __attribute__ ((aligned));

struct B2_a_max {
  char c;
  struct A2_a_max a2;
} __attribute__ ((aligned));

struct A3_a_max {
  double d;
  int i;
} __attribute__ ((aligned));

struct B3_a_max {
  char c;
  struct A3_a_max a3;
} __attribute__ ((aligned));

#define DESC_m_outer_p_inner "maximum alignment for outer, packed inner"
struct B1_m_outer_p_inner {
  char c;
  double d;
} __attribute__ ((aligned)) __attribute__ ((packed));

struct A2_m_outer_p_inner {
  double d;
} __attribute__ ((packed));

struct B2_m_outer_p_inner {
  char c;
  struct A2_m_outer_p_inner a2;
} __attribute__ ((aligned));

struct A3_m_outer_p_inner {
  double d;
  int i;
} __attribute__ ((packed));

struct B3_m_outer_p_inner {
  char c;
  struct A3_m_outer_p_inner a3;
} __attribute__ ((aligned));

#define DESC_m_inner_p_outer "maximum alignment for inner, packed outer"
struct B1_m_inner_p_outer {
  char c;
  double d;
} __attribute__ ((aligned)) __attribute__ ((packed));

struct A2_m_inner_p_outer {
  double d;
} __attribute__ ((aligned));

struct B2_m_inner_p_outer {
  char c;
  struct A2_m_inner_p_outer a2;
} __attribute__ ((packed));

struct A3_m_inner_p_outer {
  double d;
  int i;
} __attribute__ ((aligned));

struct B3_m_inner_p_outer {
  char c;
  struct A3_m_inner_p_outer a3;
} __attribute__ ((packed));
#endif
