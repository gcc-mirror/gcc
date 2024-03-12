// { dg-do compile }
// { dg-require-effective-target lp64 }

enum profile_quality { GUESSED_LOCAL };
struct profile_count {
  long m_val : 61;
  profile_quality m_quality : 3;
  bool verify() {
    m_quality ? __builtin_abort (), 0 : 0;
    return m_val == GUESSED_LOCAL;
  }
} count;
void verify_flow_info() { count.verify(); }
