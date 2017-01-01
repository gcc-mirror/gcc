/* Array and structure constructors
   Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "constructor.h"


static void
node_free (splay_tree_value value)
{
  gfc_constructor *c = (gfc_constructor*)value;

  if (c->expr)
    gfc_free_expr (c->expr);

  if (c->iterator)
    gfc_free_iterator (c->iterator, 1);

  mpz_clear (c->offset);
  mpz_clear (c->repeat);

  free (c);
}


static gfc_constructor *
node_copy (splay_tree_node node, void *base)
{
  gfc_constructor *c, *src = (gfc_constructor*)node->value;

  c = XCNEW (gfc_constructor);
  c->base = (gfc_constructor_base)base;
  c->expr = gfc_copy_expr (src->expr);
  c->iterator = gfc_copy_iterator (src->iterator);
  c->where = src->where;
  c->n.component = src->n.component;

  mpz_init_set (c->offset, src->offset);
  mpz_init_set (c->repeat, src->repeat);

  return c;
}


static int
node_copy_and_insert (splay_tree_node node, void *base)
{
  int n = mpz_get_si (((gfc_constructor*)node->value)->offset);
  gfc_constructor_insert ((gfc_constructor_base*)base,
			  node_copy (node, base), n);
  return 0;
}


gfc_constructor *
gfc_constructor_get (void)
{
  gfc_constructor *c = XCNEW (gfc_constructor);
  c->base = NULL;
  c->expr = NULL;
  c->iterator = NULL;

  mpz_init_set_si (c->offset, 0);
  mpz_init_set_si (c->repeat, 1);

  return c;
}

gfc_constructor_base gfc_constructor_get_base (void)
{
  return splay_tree_new (splay_tree_compare_ints, NULL, node_free);
}


gfc_constructor_base
gfc_constructor_copy (gfc_constructor_base base)
{
  gfc_constructor_base new_base;

  if (!base)
    return NULL;

  new_base = gfc_constructor_get_base ();
  splay_tree_foreach (base, node_copy_and_insert, &new_base);

  return new_base;
}


void
gfc_constructor_free (gfc_constructor_base base)
{
  if (base)
    splay_tree_delete (base);
}


gfc_constructor *
gfc_constructor_append (gfc_constructor_base *base, gfc_constructor *c)
{
  int offset = 0;
  if (*base)
    offset = (int)(splay_tree_max (*base)->key) + 1;

  return gfc_constructor_insert (base, c, offset);
}


gfc_constructor *
gfc_constructor_append_expr (gfc_constructor_base *base,
			     gfc_expr *e, locus *where)
{
  gfc_constructor *c = gfc_constructor_get ();
  c->expr = e;
  if (where)
    c->where = *where;

  return gfc_constructor_append (base, c);
}


gfc_constructor *
gfc_constructor_insert (gfc_constructor_base *base, gfc_constructor *c, int n)
{
  splay_tree_node node;

  if (*base == NULL)
    *base = splay_tree_new (splay_tree_compare_ints, NULL, node_free);

  c->base = *base;
  mpz_set_si (c->offset, n);

  node = splay_tree_insert (*base, (splay_tree_key) n, (splay_tree_value) c);
  gcc_assert (node);

  return (gfc_constructor*)node->value;
}


gfc_constructor *
gfc_constructor_insert_expr (gfc_constructor_base *base,
			     gfc_expr *e, locus *where, int n)
{
  gfc_constructor *c = gfc_constructor_get ();
  c->expr = e;
  if (where)
    c->where = *where;

  return gfc_constructor_insert (base, c, n);
}


gfc_constructor *
gfc_constructor_lookup (gfc_constructor_base base, int offset)
{
  gfc_constructor *c;
  splay_tree_node node;

  if (!base)
    return NULL;

  node = splay_tree_lookup (base, (splay_tree_key) offset);
  if (node)
    return (gfc_constructor *) node->value;

  /* Check if the previous node has a repeat count big enough to
     cover the offset looked for.  */
  node = splay_tree_predecessor (base, (splay_tree_key) offset);
  if (!node)
    return NULL;

  c = (gfc_constructor *) node->value;
  if (mpz_cmp_si (c->repeat, 1) > 0)
    {
      if (mpz_get_si (c->offset) + mpz_get_si (c->repeat) <= offset)
	c = NULL;
    }
  else
    c = NULL;

  return c;
}


gfc_expr *
gfc_constructor_lookup_expr (gfc_constructor_base base, int offset)
{
  gfc_constructor *c = gfc_constructor_lookup (base, offset);
  return c ? c->expr : NULL;
}


int
gfc_constructor_expr_foreach (gfc_constructor *ctor ATTRIBUTE_UNUSED,
			      int(*f)(gfc_expr *) ATTRIBUTE_UNUSED)
{
  gcc_assert (0);
  return 0;
}

void
gfc_constructor_swap (gfc_constructor *ctor ATTRIBUTE_UNUSED,
                      int n ATTRIBUTE_UNUSED, int m ATTRIBUTE_UNUSED)
{
  gcc_assert (0);
}



gfc_constructor *
gfc_constructor_first (gfc_constructor_base base)
{
  if (base)
    {
      splay_tree_node node = splay_tree_min (base);
      return node ? (gfc_constructor*) node->value : NULL;
    }
  else
    return NULL;
}


gfc_constructor *
gfc_constructor_next (gfc_constructor *ctor)
{
  if (ctor)
    {
      splay_tree_node node = splay_tree_successor (ctor->base,
						   mpz_get_si (ctor->offset));
      return node ? (gfc_constructor*) node->value : NULL;
    }
  else
    return NULL;
}


void
gfc_constructor_remove (gfc_constructor *ctor)
{
  if (ctor)
    splay_tree_remove (ctor->base, mpz_get_si (ctor->offset));
}


gfc_constructor *
gfc_constructor_lookup_next (gfc_constructor_base base, int offset)
{
  splay_tree_node node;

  if (!base)
    return NULL;

  node = splay_tree_successor (base, (splay_tree_key) offset);
  if (!node)
    return NULL;

  return (gfc_constructor *) node->value;
}
