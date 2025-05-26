/* Definition of data structure of RISC-V subset for GNU compiler.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).
   Based on MIPS target for GNU compiler.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_RISCV_SUBSET_H
#define GCC_RISCV_SUBSET_H

#include "riscv-feature-bits.h"

#define RISCV_DONT_CARE_VERSION -1

/* Subset info.  */
struct riscv_subset_t
{
  riscv_subset_t ();

  std::string name;
  int major_version;
  int minor_version;
  struct riscv_subset_t *next;

  bool explicit_version_p;
  bool implied_p;
};

/* Subset list.  */
class riscv_subset_list
{
public:
  /* Because the parse method is called in several places, to prevent repeated
     errors, use this flag to prevent it from repeating parse. */
  static bool parse_failed;

private:
  /* Original arch string.  */
  const char *m_arch;

  /* Location of arch string, used for report error.  */
  location_t m_loc;

  /* Head of subset info list.  */
  riscv_subset_t *m_head;

  /* Tail of subset info list.  */
  riscv_subset_t *m_tail;

  /* X-len of m_arch. */
  unsigned m_xlen;

  /* Number of subsets. */
  unsigned m_subset_num;

  /* Allow adding the same extension more than once.  */
  bool m_allow_adding_dup;

  riscv_subset_list (const char *, location_t);

  const char *parsing_subset_version (const char *, const char *, unsigned *,
				      unsigned *, bool, bool *);

  const char *parse_base_ext (const char *);

  const char *parse_single_std_ext (const char *, bool);

  const char *parse_single_multiletter_ext (const char *, const char *,
					    const char *, bool);

  std::string parse_profiles (const char*);

  void handle_implied_ext (const char *);
  bool check_implied_ext ();
  void handle_combine_ext ();
  void check_conflict_ext ();

public:
  ~riscv_subset_list ();

  void add (const char *, int, int, bool, bool);

  void add (const char *, bool);

  riscv_subset_t *lookup (const char *,
			  int major_version = RISCV_DONT_CARE_VERSION,
			  int minor_version = RISCV_DONT_CARE_VERSION) const;

  std::string to_string (bool) const;

  unsigned xlen () const {return m_xlen;};

  riscv_subset_list *clone () const;

  static riscv_subset_list *parse (const char *, location_t);
  const char *parse_single_ext (const char *, bool exact_single_p = true);

  int match_score (riscv_subset_list *) const;

  void set_loc (location_t);

  void set_allow_adding_dup (bool v) { m_allow_adding_dup = v; }

  void finalize ();

  class iterator
  {
  public:
    explicit iterator(riscv_subset_t *node) : m_node(node) {}

    riscv_subset_t &operator*() const { return *m_node; }
    riscv_subset_t *operator->() const { return m_node; }

    iterator &operator++()
    {
      if (m_node)
	m_node = m_node->next;
      return *this;
    }

    bool operator!=(const iterator &other) const
    {
      return m_node != other.m_node;
    }

    bool operator==(const iterator &other) const
    {
      return m_node == other.m_node;
    }

  private:
    riscv_subset_t *m_node;
  };

  iterator begin() { return iterator(m_head); }
  iterator end()   { return iterator(nullptr); }

  class const_iterator
  {
  public:
    explicit const_iterator(const riscv_subset_t *node) : m_node(node) {}

    const riscv_subset_t &operator*() const { return *m_node; }
    const riscv_subset_t *operator->() const { return m_node; }

    const_iterator &operator++()
    {
      if (m_node)
	m_node = m_node->next;
      return *this;
    }

    bool operator!=(const const_iterator &other) const
    {
      return m_node != other.m_node;
    }

  private:
    const riscv_subset_t *m_node;
  };

  const_iterator begin() const { return const_iterator(m_head); }
  const_iterator end() const   { return const_iterator(nullptr); }
};

extern const riscv_subset_list *riscv_cmdline_subset_list (void);
extern void
riscv_set_arch_by_subset_list (riscv_subset_list *, struct gcc_options *);
extern bool riscv_minimal_hwprobe_feature_bits (const char *,
						struct riscv_feature_bits *,
						location_t);
extern bool
riscv_ext_is_subset (struct cl_target_option *, struct cl_target_option *);

#endif /* ! GCC_RISCV_SUBSET_H */
