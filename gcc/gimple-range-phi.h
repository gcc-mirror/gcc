/* Header file for gimple range phi analysis.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_SSA_RANGE_PHI_H
#define GCC_SSA_RANGE_PHI_H

// -------------------------------------------------------------------------

// A PHI_GROUP consists of a set of SSA_NAMES which are all PHI_DEFS, and
// their arguemnts contain nothing but other PHI defintions, with at most
// 2 exceptions:
//  1 - An initial value.  This is either a constant, or another non-phi name
//      with a single incoming edge to the cycle group
//  2 - A modifier statement which adjusts the value.  ie, name2 = phi_name + 1
//  The initial range is used to create one bound and the modifier is examined
//  to determine the other bound.
//  All members of the PHI cycle will be given the same range.
//
// For example, given the follwoing sequences:
// qa_20 = qa_10 + 1;
// qa_9 = PHI <qa_10(3), qa_20(4)>
// qa_10 = PHI <0(2), qa_9(5)>
//
// We can determine the following group:
//
// PHI cycle members qa_9, qa_10
// Initial value : 0
// modifier stmt: qa_20 = qa_10 + 1;
//
// Based on just this analysis, We can project that qa_9 and qa_10 will have
// a range of [0, +INF].

class phi_group
{
public:
  phi_group (bitmap bm, irange &init_range, gimple *mod, range_query *q);
  phi_group (const phi_group &g);
  const_bitmap group () const { return m_group; }
  const vrange &range () const { return m_vr; }
  gimple *modifier_stmt () const { return m_modifier; }
  void dump (FILE *);
protected:
  bool calculate_using_modifier (range_query *q);
  bool refine_using_relation (relation_kind k);
  static unsigned is_modifier_p (gimple *s, const bitmap bm);
  bitmap m_group;
  gimple *m_modifier;     // Single stmt which modifies phi group.
  unsigned m_modifier_op; // Operand of group member in modifier stmt.
  int_range_max m_vr;
  friend class phi_analyzer;
};

// The phi anlyzer will return the group that name belongs to.
// If inforamtion is not known about a name yet, analysis is conducted by
// looking at the arguments to PHIS and following them to their defs to
// determine whether the conditions are met to form a new group.

class phi_analyzer
{
public:
  phi_analyzer (range_query &);
  ~phi_analyzer ();
  phi_group *operator[] (tree name);
  void dump (FILE *f);
protected:
  phi_group *group (tree name) const;
  void process_phi (gphi *phi);
  range_query &m_global;
  vec<tree> m_work;

  bitmap m_simple;       // Processed, not part of a group.
  bitmap m_current;	 // Potential group currently being analyzed.
  vec<phi_group *> m_phi_groups;
  vec<phi_group *> m_tab;
  bitmap_obstack m_bitmaps;
};

// These are the APIs to start and stop a phi analyzerin a SCEV like manner.
// There can only be one operating at any given time.
// When initialized, a range-query if provided to do lookups of values for
// PHIs and to evaluate modifier and initial value statements.
// To avoid problems, this should be some form of constant query, like
// global_range_query or better yet a const_query from a functioning ranger.

bool phi_analysis_available_p ();
phi_analyzer &phi_analysis ();
void phi_analysis_initialize (range_query &);
void phi_analysis_finalize ();

#endif // GCC_SSA_RANGE_PHI_H
