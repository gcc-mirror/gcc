/* Struct-reorg optimization.
   Copyright (C) 2002, 2003-2007 Free Software Foundation, Inc.
   Contributed by Olga Golovanevsky <olga@il.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef IPA_STRUCT_REORG_H
#define IPA_STRUCT_REORG_H

/* This file contains data structures and interfaces required
   for struct-reorg optimizations.  */

/* An access site of the structure field.
   We consider an access to be of the following form:

   D.2166_21 = i.6_20 * 8;
   D.2167_22 = (struct str_t *) D.2166_21;
   D.2168_24 = D.2167_22 + p.5_23;
   D.2169_25 = D.2168_24->b;
*/

struct field_access_site
{
  /* Statement in which the access site occurs.  */
  tree stmt;             /* D.2169_25 = D.2168_24->b;  */
  tree comp_ref;         /* D.2168_24->b  */
  tree field_decl;       /* b */
  tree ref;              /* D.2168_24  */
  tree num;              /* i.6_20  */
  tree offset;           /* D2167_22  */
  tree base;             /* p.5_23  */
  tree ref_def_stmt;     /* D.2168_24 = D.2167_22 + p.5_23;  */
  tree cast_stmt;        /* D.2167_22 = (struct str_t *) D.2166_21;
                            This statement is not always present.  */
};

/* A non-field structure access site.  */
struct access_site
{
  /* A statement in which the access site occurs.  */
  tree stmt;
  /* A list of structure variables in the access site.  */
  VEC (tree, heap) *vars;
};

/* A field of the structure.  */
struct field_entry
{
  /* A field index.  */
  int index;
  /* Number of times the field is accessed (according to profiling).  */
  gcov_type count;
  tree decl;
  /* A type of a new structure this field belongs to.  */
  tree field_mapping;
  htab_t acc_sites;
};

/* This structure represents a result of the structure peeling.
   The original structure is decomposed into substructures, or clusters.  */
struct field_cluster
{
  /* A bitmap of field indices. The set bit indicates that the field 
     corresponding to it is a part of this cluster.  */
  sbitmap fields_in_cluster;
  struct field_cluster *sibling;
};

/* An information about an individual structure type (RECORD_TYPE) required
   by struct-reorg optimizations to perform a transformation.  */
struct data_structure
{

  /* A main variant of the structure type.  */
  tree decl;

  /* Number of fields in the structure.  */
  int num_fields;

  /* A structure access count collected through profiling.  */
  gcov_type count;

  /* An array of the structure fields, indexed by field ID.  */
  struct field_entry *fields;

  /* Non-field accesses of the structure.  */
  htab_t accs;

  /* A data structure representing a reorganization decision.  */
  struct field_cluster *struct_clustering;

  /* New types to replace an the original structure type.  */
  VEC(tree, heap) *new_types;
};

typedef struct data_structure * d_str;

#endif /* IPA_STRUCT_REORG_H */
