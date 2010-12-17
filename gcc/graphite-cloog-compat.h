/* Compatibility layer for using upstream CLooG versions with
   CLooG legacy code.
   Copyright (C) 2010 Free Software Foundation, Inc.
   Contributed by Andreas Simbuerger <simbuerg@fim.uni-passau.de>.

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

#ifndef GRAPHITE_CLOOG_COMPAT_H
#define GRAPHITE_CLOOG_COMPAT_H

/* Restore compatibility to CLooG Legacy.  */
#ifdef CLOOG_ORG
typedef const struct clast_expr *clast_name_p;
#else
typedef const char *clast_name_p;
#endif

#ifdef CLOOG_ORG
#define cloog_initialize()
#define cloog_finalize()
#endif

#ifndef CLOOG_ORG

/* CloogOptions compatibility.  */
#define build_cloog_prog(SCOP, PROG, OPT, STATE)\
  build_cloog_prog (SCOP, PROG, STATE)
#define cloog_program_extract_scalars(PROG, SCATT, OPT)\
  cloog_program_extract_scalars (PROG, SCATT)
#define cloog_program_scatter(PROG, SCATT, OPT)\
  cloog_program_scatter (PROG, SCATT)

/* CLAST compatibility.  */
#define clast_expr_term expr_term
#define clast_expr_red expr_red
#define clast_expr_bin expr_bin
#define clast_pprint pprint

/* CloogState compatibility.  */
#define CloogState void
#define cloog_state_malloc() NULL
#define cloog_state_free(STATE)
#define cloog_loop_malloc(STATE) cloog_loop_malloc ()
#define cloog_options_malloc(STATE) cloog_options_malloc ()
#define cloog_statement_alloc(STATE, INDEX) cloog_statement_alloc (INDEX)
#define new_Cloog_Domain_from_ppl_Pointset_Powerset(PSPS, NB, STATE)\
  new_Cloog_Domain_from_ppl_Pointset_Powerset (PSPS)
#define new_Cloog_Domain_from_ppl_Polyhedron(POLY, NB, STATE)\
  new_Cloog_Domain_from_ppl_Polyhedron (POLY)
#define cloog_domain_from_cloog_matrix(STATE, MAT, NB)\
  cloog_domain_matrix2domain (MAT)

/* CloogScatteringList compatibility.  */
#define CloogScatteringList CloogDomainList
#define CloogScattering CloogDomain
#define cloog_set_next_scattering cloog_set_next_domain
#define cloog_set_scattering cloog_set_domain
#define cloog_scattering cloog_domain
#define cloog_next_scattering cloog_next_domain
#define cloog_scattering_free cloog_domain_free
#define cloog_program_dump_cloog(DUMPFILE, PROGRAM, SCATTERINGLIST)\
  cloog_program_dump_cloog (DUMPFILE, PROGRAM)

#endif

/* Adapt CLooG accessors from CLooG legacy to
   newer CLooG versions.  */

#ifdef CLOOG_ORG

static inline void *
cloog_statement_usr (CloogStatement *cs)
{
  return cs->usr;
}

static inline CloogScattering *
cloog_scattering (CloogScatteringList *sl)
{
  return sl->scatt;
}

static inline void
cloog_set_scattering (CloogScatteringList *sl, CloogScattering *scatt)
{
  sl->scatt = scatt;
}

static inline CloogScatteringList *
cloog_next_scattering (CloogScatteringList *sl)
{
  return sl->next;
}

static inline void
cloog_set_next_scattering (CloogScatteringList *sl, CloogScatteringList *next)
{
  sl->next = next;
}

static inline int
cloog_program_nb_scattdims (CloogProgram *prog)
{
  return prog->nb_scattdims;
}

static inline void
cloog_program_set_nb_scattdims (CloogProgram *prog, int nb_scattdims)
{
  prog->nb_scattdims = nb_scattdims;
}

static inline CloogNames *
cloog_program_names (CloogProgram *prog)
{
  return prog->names;
}

static inline void
cloog_program_set_names (CloogProgram *prog, CloogNames *names)
{
  prog->names = names;
}

static inline void
cloog_program_set_context (CloogProgram *prog, CloogDomain *domain)
{
  prog->context = domain;
}

static inline void
cloog_program_set_loop (CloogProgram *prog, CloogLoop *loop)
{
  prog->loop = loop;
}

static inline CloogBlockList *
cloog_program_blocklist (CloogProgram *prog)
{
  return prog->blocklist;
}

static inline void
cloog_program_set_blocklist (CloogProgram *prog, CloogBlockList *bl)
{
  prog->blocklist = bl;
}

static inline int *
cloog_program_scaldims (CloogProgram *prog)
{
  return prog->scaldims;
}

static inline void
cloog_program_set_scaldims (CloogProgram *prog, int *s)
{
  prog->scaldims = s;
}

static inline int
cloog_names_nb_parameters (CloogNames *names)
{
  return names->nb_parameters;
}

static inline void
cloog_names_set_nb_parameters (CloogNames *names, int nb_parameters)
{
  names->nb_parameters = nb_parameters;
}

static inline char **
cloog_names_parameters (CloogNames *names)
{
  return names->parameters;
}

static inline void
cloog_names_set_parameters (CloogNames *names, char **parameters)
{
  names->parameters = parameters;
}

static inline void
cloog_names_set_nb_iterators (CloogNames *names, int nb_iterators)
{
  names->nb_iterators = nb_iterators;
}

static inline void
cloog_names_set_iterators (CloogNames *names, char **iterators)
{
  names->iterators = iterators;
}

static inline void
cloog_names_set_nb_scattering (CloogNames *names, int nb_scattering)
{
  names->nb_scattering = nb_scattering;
}

static inline void
cloog_names_set_scattering (CloogNames *names, char **scattering)
{
  names->scattering = scattering;
}

static inline void
cloog_statement_set_usr (CloogStatement *cs, void *u)
{
  cs->usr = u;
}

static inline void
cloog_loop_set_next (CloogLoop *loop, CloogLoop *next)
{
  loop->next = next;
}

static inline void
cloog_loop_set_domain (CloogLoop *loop, CloogDomain *domain)
{
  loop->domain = domain;
}

static inline void
cloog_loop_set_block (CloogLoop *loop, CloogBlock *block)
{
  loop->block = block;
}

static inline CloogBlockList *
cloog_block_list_next (CloogBlockList *bl)
{
  return bl->next;
}

static inline void
cloog_block_list_set_next (CloogBlockList *bl, CloogBlockList *next)
{
  bl->next = next;
}

static inline void
cloog_block_list_set_block (CloogBlockList *bl, CloogBlock *block)
{
  bl->block = block;
}

static inline int cloog_matrix_ncolumns (CloogMatrix * m)
{
  return m->NbColumns;
}

static inline int cloog_matrix_nrows (CloogMatrix * m)
{
   return m->NbRows;
}
#endif /* CLOOG_ORG  */
#endif /* GRAPHITE_CLOOG_COMPAT_H  */
