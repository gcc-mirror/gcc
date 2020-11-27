/* This file is part of GCC.

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
   <http://www.gnu.org/licenses/>. */

#include "rust.h"

static std::vector<std::map<std::string, rdot> *> context;
static rdot dot_pass_typeifyExprNode (rdot);
static bool dot_pass_typeCompare (const rdot, const rdot);
static rdot impl_master = NULL_DOT;

static
void dot_pass_dataFlow_pushCtx (void)
{
  std::map<std::string, rdot> * ctx = new std::map<std::string, rdot>;
  context.push_back (ctx);
}

static
void dot_pass_dataFlow_popCtx (void)
{
  std::map<std::string, rdot> * ctx = context.back ();
  context.pop_back ();
  delete ctx;
}

static
rdot dot_pass_dataFlow_lookup (const char * id)
{
  rdot retval = NULL_DOT;
  std::vector<std::map<std::string, rdot> *>::reverse_iterator it;
  for (it = context.rbegin (); it != context.rend (); ++it)
    {
      std::map<std::string, rdot> * ctx = *it;
      if (ctx->count (std::string (id)))
        {
          retval = (*ctx)[std::string (id)];
          break;
        }
    }
  return retval;
}

static
bool dot_pass_dataFlow_pushDecl (rdot node, const char * id)
{
  rdot check = dot_pass_dataFlow_lookup (id);
  if (check != NULL_DOT)
    {
      error ("DataFlow duplicate declaration [%s]\n", id);
      return true;
    }

  bool retval = false;
  if ((RDOT_TYPE (node) == D_VAR_DECL)
      || (RDOT_TYPE (node) == D_STRUCT_METHOD)
      || (RDOT_TYPE (node) == D_STRUCT_TYPE))
    {
      std::map<std::string, rdot> * ctx = context.back ();
      (*ctx) [std::string (id)] = node;
    }
  else
    {
      error ("Invalid dataflow declaration pushing to context [%s]\n",
             RDOT_OPCODE_STR (node));
      retval = true;
    }
  return retval;
}

static void dot_pass_dataFlowToplevel (rdot);
static void dot_pass_dataFlowFunction (rdot);
static void dot_pass_dataFlowBlock (rdot);

static
bool verifyType (rdot node)
{
  bool retval = false;
  switch (RDOT_TYPE (node))
    {
    case RTYPE_BOOL:
    case RTYPE_INT:
    case RTYPE_FLOAT:
    case RTYPE_UINT:
      retval = true;
      break;
        
    default:
      break;
    }
  return retval;
}

static
rdot dot_pass_dataFlow_getDecl (rdot node)
{
  rdot retval = NULL_DOT;
  if (RDOT_T_FIELD (node) == D_D_EXPR)
    {
      // only if its a modify expr
      if (RDOT_TYPE (node) == D_MODIFY_EXPR)
        {
          rdot decl = RDOT_lhs_TT (node);
          if (RDOT_TYPE (decl) == D_VAR_DECL)
            retval = decl;
        }
      else if (RDOT_TYPE (node) == D_VAR_DECL)
        retval = node;
    }
  return retval;
}

static
void dot_pass_dataFlowBlock_retvals (rdot suite, std::vector<rdot> * retval)
{
  rdot next;
  for (next = suite; next != NULL_DOT; next = RDOT_CHAIN (next))
    {
      if (RDOT_T_FIELD (next) == D_D_EXPR)
	{
	  if (DOT_RETVAL (next))
	    retval->push_back (next);
	}
      else
	{
	  switch (RDOT_TYPE (next))
	    {
	    case D_STRUCT_WHILE:
	      dot_pass_dataFlowBlock_retvals (RDOT_rhs_TT (next), retval);
	      break;

	    case D_STRUCT_IF:
	      {
		rdot ifblock = RDOT_lhs_TT (next);
		rdot elseblock = RDOT_rhs_TT (next);
		dot_pass_dataFlowBlock_retvals (RDOT_rhs_TT (ifblock), retval);
		if (elseblock != NULL_DOT)
		  dot_pass_dataFlowBlock_retvals (RDOT_lhs_TT (elseblock), retval);
	      }
	      break;

	    default:
	      error ("unable to figure out what to do with [%s]",
		     RDOT_OPCODE_STR (next));
	      break;
	    }
	}
    }
}

/*
  Takes a var_decl and returns the rhs of assignment to try and infer a type on.
  eg1 :
  let x;
  x = 1 + 2
  will return the NULL on the decl and RDOT (1+2) on the expression

  eg2:
  let x = 1;
  ruturns RDOT (1)
*/
static
rdot dot_pass_dataFlow_getRef (rdot decl, rdot var_decl)
{
  rdot retval = NULL_DOT;
  if (RDOT_TYPE (decl) == D_MODIFY_EXPR)
    {
      rdot lhs = RDOT_lhs_TT (decl);
      rdot rhs = RDOT_rhs_TT (decl);

      switch (RDOT_TYPE (lhs))
        {
        case D_VAR_DECL:
          {
            if (var_decl == lhs)
              retval = rhs;
          }
          break;

        case D_IDENTIFIER:
          {
            const char * vid = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (var_decl));
            const char * cid = RDOT_IDENTIFIER_POINTER (lhs);
            // we found a reference assignment...
            if (strcmp (vid, cid) == 0)
              retval = rhs;
          }
          break;

        default:
          break;
        }
    }
  return retval;
}

static
std::vector<rdot> * dot_pass_getReferences (rdot vDecl, rdot suite)
{
  std::vector<rdot> * retval = new std::vector<rdot>;
  rdot node;
  for (node = suite; node != NULL_DOT; node = RDOT_CHAIN (node))
    {
      switch (RDOT_TYPE (node))
	{
	case D_PRIMITIVE:
	case D_CALL_EXPR:
	case D_ATTRIB_REF:
	  break;

	case D_MODIFY_EXPR:
	  {
	    rdot ref = dot_pass_dataFlow_getRef (node, vDecl);
	    if (ref != NULL_DOT)
	      retval->push_back (ref);
	  }
	  break;

	case D_STRUCT_WHILE:
	  {
	    rdot wsuite = RDOT_rhs_TT (node);
	    std::vector<rdot> * refs = dot_pass_getReferences (vDecl, wsuite);
            // append to the list
	    std::vector<rdot>::iterator it;
	    for (it = refs->begin (); it != refs->end (); ++it)
              retval->push_back (*it);
	    delete refs;
	  }
	  break;

	case D_STRUCT_IF:
	  {
	    rdot ifb = RDOT_lhs_TT (node);
	    rdot elb = RDOT_rhs_TT (node);

	    rdot ifsuite = RDOT_rhs_TT (ifb);
	    std::vector<rdot> * refs = dot_pass_getReferences (vDecl, ifsuite);

	    // append to the list
	    std::vector<rdot>::iterator it;
	    for (it = refs->begin (); it != refs->end (); ++it)
              retval->push_back (*it);
	    delete refs;

	    if (elb != NULL_DOT)
	      {
		refs = dot_pass_getReferences (vDecl, RDOT_lhs_TT (elb));
		for (it = refs->begin (); it != refs->end (); ++it)
		  retval->push_back (*it);
		delete refs;
	      }
	  }

	default:
	  break;
	}
    }
  return retval;
}

static bool
dot_pass_typeCompare (const rdot x, const rdot y)
{
  bool retval = false;
  if (RDOT_TYPE (x) == RDOT_TYPE (y))
    if (RDOT_MEM_MODIFIER (x)->size () == RDOT_MEM_MODIFIER (y)->size ())
      {
        retval = true;
        std::vector<ALLOCA_>::iterator xit;
        std::vector<ALLOCA_>::iterator yit;
        for (xit = RDOT_MEM_MODIFIER (x)->begin (),
               yit = RDOT_MEM_MODIFIER (y)->begin ();
             xit != RDOT_MEM_MODIFIER (x)->end ();
             ++xit, ++yit)
          {
            if (*xit != *yit)
              {
                retval = false;
                break;
              }
          }
      }
  return retval;
}

static char *
dot_pass_typeString (const rdot node)
{
  char buffer [128];
  size_t offset = 0;

  std::vector<ALLOCA_>::iterator it;
  for (it = RDOT_MEM_MODIFIER (node)->begin ();
       it != RDOT_MEM_MODIFIER (node)->end (); ++it )
    {
      switch (*it)
	{
	case ALLOC_DEREF:
	  buffer [offset++] = '*';
	  break;
	case ALLOC_HEAP:
	  buffer [offset++] = '~';
	  break;
	case ALLOC_REF:
	  buffer [offset++] = '&';
	  break;
	}
    }
  strcpy (buffer+offset, RDOT_OPCODE_STR (node));
  return xstrdup (buffer);
}

/**
 * WARN:
 *   This really needs more analysis on why, if you have : infered something has:
 *   infer, int, string
 *   infer, int, infer. Does it mean its an int probably.
 *   Currently it drops any possible type to be an int
 **/
static
rdot dot_pass_inferTheType (std::vector<rdot> * refs, const char * id)
{
  rdot retval = NULL_DOT;
  rdot _retval = rdot_build_decl1 (RTYPE_INFER, NULL_DOT);
  gcc_assert (refs->size () > 0);

  std::vector<rdot> possible_types;
  std::vector<rdot>::iterator it;
  for (it = refs->begin (); it != refs->end (); ++it)
    {
      rdot pos = dot_pass_typeifyExprNode (*it);
      if (RDOT_TYPE (pos) != RTYPE_INFER)
	possible_types.push_back (pos);
    }

  if (possible_types.size () == 0)
    retval = _retval;
  else
    {
      bool first = true;
      std::vector<rdot>::iterator pit;
      for (pit = possible_types.begin (); pit != possible_types.end (); ++pit)
	{
	  if (first == true)
	    {
	      retval = *pit;
	      first = false;
	    }          
	  if (!dot_pass_typeCompare (retval, *pit))
	    {
              char * t1 = dot_pass_typeString (retval);
              char * t2 = dot_pass_typeString (*pit);
              error ("Ambigious types found for [%s] -> [%s] OR [%s]", id, t1, t2);
	      retval = _retval;
              free (t1);
              free (t2);
	      break;
	    }
	}
    }
  return retval;
}

static
rdot dot_pass_typeifyPrimitive (rdot node)
{
  rdot retval = rdot_build_decl1 (RTYPE_INFER, NULL_DOT);
  gcc_assert (RDOT_TYPE (node) == D_PRIMITIVE);

  switch (node->opa.tc.T == D_T_INTEGER)
    {
    case D_T_INTEGER:
      RDOT_TYPE (retval) = RTYPE_INT;
      break;

    default:
      error ("Unable to figure out type for this primitive [%s]!",
             RDOT_CODE_STR (node->opa.tc.T));
      break;
    }
  std::vector<ALLOCA_>::iterator it;
  for (it = RDOT_MEM_MODIFIER (node)->begin ();
       it != RDOT_MEM_MODIFIER (node)->end (); ++it)
    RDOT_MEM_MODIFIER (retval)->push_back (*it);
  return retval;
}

/* FIXME maybe i feel this isn't done very well at all here */
static
rdot dot_pass_typeifyExprNode (rdot node)
{
  rdot retval = rdot_build_decl1 (RTYPE_INFER, NULL_DOT);
  switch (RDOT_TYPE (node))
    {
    case D_PRIMITIVE:
      retval = dot_pass_typeifyPrimitive (node);
      break;

    case D_IDENTIFIER:
      {
        rdot lookup = dot_pass_dataFlow_lookup (RDOT_IDENTIFIER_POINTER (node));
        if (lookup == NULL_DOT)
          error ("unable to find declaration of [%s] in current scope",
                 RDOT_IDENTIFIER_POINTER (node));
        else
          {
            gcc_assert (RDOT_TYPE (lookup) == D_VAR_DECL);
            RDOT_TYPE (retval) = RDOT_TYPE (RDOT_rhs_TT (lookup));
            if (RDOT_TYPE (retval) == RTYPE_USER_STRUCT)
              {
                RDOT_lhs_TT (retval) = RDOT_lhs_TT (RDOT_rhs_TT (lookup));
                RDOT_rhs_TT (retval) = RDOT_rhs_TT (RDOT_rhs_TT (lookup));
              }
            std::vector<ALLOCA_>::iterator it;
            if (RDOT_MEM_MODIFIER (node))
              for (it = RDOT_MEM_MODIFIER (node)->begin ();
                   it != RDOT_MEM_MODIFIER (node)->end (); ++it)
                RDOT_MEM_MODIFIER (retval)->push_back (*it);
            if (RDOT_MEM_MODIFIER (RDOT_rhs_TT (lookup)))
              for (it = RDOT_MEM_MODIFIER (RDOT_rhs_TT (lookup))->begin ();
                   it != RDOT_MEM_MODIFIER (RDOT_rhs_TT (lookup))->end (); ++it)
                RDOT_MEM_MODIFIER (retval)->push_back (*it);
          }
      }
      break;

    case D_STRUCT_INIT:
      {
	const char * slookup = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (node));
	rdot lookup = dot_pass_dataFlow_lookup (slookup);
	if (lookup != NULL_DOT)
	  {
	    if (RDOT_TYPE (lookup) == D_STRUCT_TYPE)
	      {
                RDOT_TYPE (retval) = RTYPE_USER_STRUCT;
                RDOT_lhs_TT (retval) = RDOT_lhs_TT (lookup); // identifier node
                RDOT_rhs_TT (retval) = RDOT_rhs_TT (lookup); // struct layout
                std::vector<ALLOCA_>::iterator it;
                if (RDOT_MEM_MODIFIER (node))
                  for (it = RDOT_MEM_MODIFIER (node)->begin ();
                       it != RDOT_MEM_MODIFIER (node)->end (); ++it)
                    RDOT_MEM_MODIFIER (retval)->push_back (*it);
                if (RDOT_MEM_MODIFIER (lookup))
                  for (it = RDOT_MEM_MODIFIER (lookup)->begin ();
                       it != RDOT_MEM_MODIFIER (lookup)->end (); ++it)
                    RDOT_MEM_MODIFIER (retval)->push_back (*it);
	      }
	    else
	      error ("unable to determine type of [%s] struct initilization, "
		     "[%s] was found in this scope",
		     slookup, RDOT_OPCODE_STR (lookup));
	  }
	else
	  error ("[%s] does not name a type in scope", slookup);
      }
      break;

    case D_CALL_EXPR:
      {
        const char * callid = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (node));
        rdot lookup = dot_pass_dataFlow_lookup (callid);
        if (lookup != NULL_DOT)
          {
            gcc_assert (RDOT_TYPE (lookup) == D_STRUCT_METHOD);
            RDOT_TYPE (retval) = RDOT_TYPE (RDOT_FIELD2 (lookup));
            std::vector<ALLOCA_> * mods = RDOT_MEM_MODIFIER (RDOT_FIELD2 (lookup));
            std::vector<ALLOCA_>::iterator it;
            if (RDOT_MEM_MODIFIER (node))
              for (it = RDOT_MEM_MODIFIER (node)->begin ();
                   it != RDOT_MEM_MODIFIER (node)->end (); ++it)
                RDOT_MEM_MODIFIER (retval)->push_back (*it);
            if (mods)
              for (it = mods->begin (); it != mods->end (); ++it)
                RDOT_MEM_MODIFIER (retval)->push_back (*it);
          }
        else
          error ("unable to find declaration of [%s] in current scope",
                 callid);
      }
      break;

    case D_ATTRIB_REF:
      {
        rdot lhs = RDOT_lhs_TT (node);
        rdot base_type = dot_pass_typeifyExprNode (lhs);
        gcc_assert (RDOT_TYPE (base_type) == RTYPE_USER_STRUCT);
      }
      break;

    case D_ACC_EXPR:
      {
	rdot impl = RDOT_lhs_TT (node);
	char * implid = RDOT_IDENTIFIER_POINTER (impl);
	rdot lookup = dot_pass_dataFlow_lookup (implid);

	bool found = false;
	rdot next;
	for (next = RDOT_rhs_TT (RDOT_FIELD (lookup));
	     next != NULL_DOT; next = RDOT_CHAIN (next))
	  {
	    switch (RDOT_TYPE (next))
	      {
	      case D_STRUCT_METHOD:
		RDOT_TYPE (retval) = RDOT_TYPE (RDOT_FIELD2 (next));
		break;

	      default:
		error_at (RDOT_LOCATION (next), "unable to identify [%s] for type inferance",
			  RDOT_OPCODE_STR (next));
		break;
	      }
	  }
	if (!found)
	  break;
      }
      break;

    case D_ADD_EXPR:
    case D_MINUS_EXPR:
    case D_MULT_EXPR:
    case D_DIVD_EXPR:
      {
        rdot lhs = RDOT_lhs_TT (node);
        rdot rhs = RDOT_rhs_TT (node);

	rdot lt = dot_pass_typeifyExprNode (lhs);
	rdot rt = dot_pass_typeifyExprNode (rhs);

	if (RDOT_TYPE (lt) != RTYPE_INFER
	    || RDOT_TYPE (rt) != RTYPE_INFER)
	  {
            if (RDOT_TYPE (lt) == RDOT_TYPE (rt))
              retval = lt;
            else
              {
                if (RDOT_TYPE (lt) == RTYPE_INFER || RDOT_TYPE (rt) == RTYPE_INFER)
                  retval = RDOT_TYPE (lt) == RTYPE_INFER ? rt : lt;
                else
                  error ("unable to coerce types [%s] and [%s]",
                         RDOT_OPCODE_STR (lt), RDOT_OPCODE_STR (rt));
              }
	  }
      }
      break;

    default:
      error ("Unable to figure out the type of this [%s]",
	     RDOT_OPCODE_STR (node));
      break;
    }

  bool skip_next = false;
  std::vector<ALLOCA_> nmods;
  std::vector<ALLOCA_> * pmods = RDOT_MEM_MODIFIER (retval);
  
  std::vector<ALLOCA_>::iterator it;
  for (it = pmods->begin (); it != pmods->end (); ++it)
    {
      switch (*it)
	{
	case ALLOC_DEREF:
	  skip_next = true;
	  break;
              
	default:
	  {
	    if (!skip_next)
	      {
		nmods.push_back (*it);
		skip_next = false;
	      }
	  }
	  break;
	}
    }
  RDOT_MMEM_COPY ((&nmods), RDOT_MEM_MODIFIER (retval));
  return retval;
}

static
void dot_pass_mutability (const rdot node)
{
  rdot lhs = RDOT_lhs_TT (node);
  if (RDOT_TYPE (lhs) != D_VAR_DECL)
    {
      // check the nodes mutability
      switch (RDOT_TYPE (lhs))
	{
	case D_IDENTIFIER:
	  {
	    const char * ident = RDOT_IDENTIFIER_POINTER (lhs);
	    const rdot node = dot_pass_dataFlow_lookup (ident);
	    if (node == NULL_DOT)
	      error_at (RDOT_LOCATION (node), "Unable to find decl [%s] in current scope", ident);
	    else if (RDOT_qual (node) == true)
	      error_at (RDOT_LOCATION (node), "Unable to modify [%s] it is immutable", ident);
	  }
	  break;

	default:
	  {
	    const char * nstr = RDOT_OPCODE_STR (lhs);
	    warning_at (RDOT_LOCATION (node), 0, "TODO unable to verify assignment"
			"mutability for [%s]", nstr);
	  }
	  break;
	}
    }
}

static
void dot_pass_dataFlowBlock_ (rdot suite, std::vector<rdot> * decls)
{
  rdot node;
  for (node = suite; node != NULL_DOT; node = RDOT_CHAIN (node))
    {
      if (RDOT_T_FIELD (node) == D_D_EXPR)
        {
	  if (RDOT_TYPE (node) == D_MODIFY_EXPR)
	    dot_pass_mutability (node);

          const char * id = NULL;
          rdot decl = dot_pass_dataFlow_getDecl (node);
          if (decl != NULL_DOT)
            {
	      gcc_assert (RDOT_TYPE (decl) == D_VAR_DECL);
	      id = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (decl));
	      decls->push_back (decl);

              // push it into the current context...
              rdot lookup = dot_pass_dataFlow_lookup (id);
              if (lookup != NULL_DOT)
                error ("Duplicate declaration of [%s] to [%s]",
                       id, RDOT_OPCODE_STR (lookup));
              else
                gcc_assert (!dot_pass_dataFlow_pushDecl (decl, id));
            }
        }
      else
	{
	  switch (RDOT_TYPE (node))
	    {
	    case D_STRUCT_IF:
	      {
		rdot sif = RDOT_lhs_TT (node);
		rdot ses = RDOT_rhs_TT (node);

		dot_pass_dataFlowBlock_ (RDOT_rhs_TT (sif), decls);
		if (ses != NULL_DOT)
		  dot_pass_dataFlowBlock_ (RDOT_lhs_TT (ses), decls);
	      }
	      break;

	    case D_STRUCT_WHILE:
	      dot_pass_dataFlowBlock_ (RDOT_rhs_TT (node), decls);
	      break;

            case D_STRUCT_LOOP:
              dot_pass_dataFlowBlock_ (RDOT_lhs_TT (node), decls);
              break;

            case C_BREAK_STMT:
            case C_CONT_STMT:
              break;

	    default:
	      error ("Unhandled data flow in block on [%s]\n", RDOT_OPCODE_STR (node));
	      break;
	    }
	}
    }
}

static
void dot_pass_dataFlowBlock (rdot suite)
{
  std::vector<rdot> block_decls;
  dot_pass_dataFlowBlock_ (suite, &block_decls);

  // now we have all defined block declarations now need to get all
  // references in their use if they have an undefined type.
  std::vector<rdot>::iterator it;
  for (it = block_decls.begin (); it != block_decls.end (); ++it)
    {
      rdot decl = *it;
      gcc_assert (RDOT_TYPE (decl) == D_VAR_DECL);
      rdot idecl = RDOT_lhs_TT (decl);
      const char * ident = RDOT_IDENTIFIER_POINTER (idecl);
      if (RDOT_TYPE (RDOT_rhs_TT (decl)) == RTYPE_INFER)
	{
	  std::vector<rdot> * refs = dot_pass_getReferences (decl, suite);
	  if (refs->size () == 0)
	    {
	      error ("Unable to infer type of [%s] it looks to "
		     "be unused in this scope", ident);
	      continue;
	    }
	  RDOT_rhs_TT (decl) = dot_pass_inferTheType (refs, ident);
	  if (RDOT_TYPE (RDOT_rhs_TT (decl)) == RTYPE_INFER)
	    error ("Compiler was unable to infer the type for [%s]", ident);
	  delete refs;
	}
    }
}

static
void dot_pass_dataFlowFunction (rdot node)
{
  const char * method_id = RDOT_IDENTIFIER_POINTER (RDOT_FIELD (node));
  rdot type = RDOT_FIELD2 (node);

  // rust does not infer function types empty types are default void
  // and parameters are synatically required to be typed
  dot_pass_dataFlow_pushCtx ();

  // fill up the parameters here
  rdot params;
  for (params = RDOT_lhs_TT (node); params != NULL_DOT;
       params = RDOT_CHAIN (params))
    {
      rdot pident = RDOT_lhs_TT (params);
      rdot ptype = RDOT_rhs_TT (params);

      const char * cpid = RDOT_IDENTIFIER_POINTER (pident);
      if (strcmp (cpid, "self") == 0)
	{
	  gcc_assert (impl_master != NULL_DOT);
	  rdot stid = RDOT_lhs_TT (impl_master);
	  ptype = rdot_build_decl1 (RTYPE_USER_STRUCT, stid);
	}
      rdot vpdecl = rdot_build_varDecl (ptype, RDOT_qual (params), pident);
      bool chk = dot_pass_dataFlow_pushDecl (vpdecl,
					     RDOT_IDENTIFIER_POINTER (pident));
      gcc_assert (chk == false);
    }

  rdot suite = RDOT_rhs_TT (node);
  dot_pass_dataFlowBlock (suite);

  // now check the return type is correct
  /* make sure its a valid type! */
  if (type != NULL)
    {
      bool verify = verifyType (type);
      if (verify == false)
        error ("unable to verify return type of %s [%s]\n",
               method_id, RDOT_OPCODE_STR (type));
      else
	{
	  std::vector<rdot> retvals;
	  dot_pass_dataFlowBlock_retvals (suite, &retvals);

	  if (retvals.size () == 0)
	    error ("Function [%s] does not seem to return anything!", method_id);
	  else
	    {
	      rdot retype = dot_pass_inferTheType (&retvals, method_id);
	      if (RDOT_TYPE (retype) == RTYPE_INFER)
		error ("gcc rust was unable to verify the return type of [%s]",
		       method_id);
	    }
	}
    }
  dot_pass_dataFlow_popCtx ();
}

static
void dot_pass_dataFlowToplevel (rdot node)
{
  switch (RDOT_TYPE (node))
    {
    case D_STRUCT_METHOD:
      dot_pass_dataFlowFunction (node);
      break;

      /* need to dataflow the layout to check the types eventually */
    case D_STRUCT_TYPE:
      break;

    case D_STRUCT_IMPL:
      {
	// look up to make sure the impl name is available...
	const char * implid = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (node));
	rdot lookup = dot_pass_dataFlow_lookup (implid);
	if (lookup == NULL_DOT)
	  error ("impl [%s] does not reference a type in scope", implid);
	else
	  {
	    if (RDOT_TYPE (lookup) != D_STRUCT_TYPE)
	      error ("impl [%s] does not reference a struct points to [%s]",
		     implid, RDOT_OPCODE_STR (lookup));
	    else
	      {
		RDOT_FIELD (lookup) = node;
		impl_master = lookup;
		rdot next;
		for (next = RDOT_rhs_TT (node); next != NULL_DOT;
		     next = RDOT_CHAIN (next))
                  dot_pass_dataFlowFunction (next);
		impl_master = NULL_DOT;
	      }
	  }
      }
      break;

    default:
      error ("Unable to dataflow %s\n", RDOT_OPCODE_STR (node));
      break;
    }
}

vec<rdot,va_gc> * dot_pass_inferTypes (vec<rdot,va_gc> * decls)
{
  dot_pass_dataFlow_pushCtx ();

  rdot idtx = NULL_DOT;
  size_t i;
  for (i = 0; decls->iterate (i, &idtx); ++i)
    {
      rdot node = idtx;
      switch (RDOT_TYPE (node))
	{
	case D_STRUCT_METHOD:
	  {
	    const char * id = RDOT_IDENTIFIER_POINTER (RDOT_FIELD (node));
	    if (dot_pass_dataFlow_lookup (id))
	      error ("Duplicate declaration against this function [%s]\n", id);
	    else
	      dot_pass_dataFlow_pushDecl (node, id);
	  }
	  break;

	case D_STRUCT_TYPE:
	  {
	    const char * id = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (node));
	    if (dot_pass_dataFlow_lookup (id))
	      error ("Duplicate declaration against this type [%s]\n", id);
	    else
	      dot_pass_dataFlow_pushDecl (node, id);
	  }
	  break;

	case D_STRUCT_IMPL:
	  {
	    // look up to make sure the impl name is available...
	    const char * implid = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (node));
	    rdot lookup = dot_pass_dataFlow_lookup (implid);
	    if (lookup == NULL_DOT)
	      error ("impl [%s] does not reference a type in scope", implid);
	    else
	      {
		if (RDOT_TYPE (lookup) != D_STRUCT_TYPE)
		  error ("impl [%s] does not reference a struct points to [%s]",
			 implid, RDOT_OPCODE_STR (lookup));
		else
		  RDOT_FIELD (lookup) = node;
	      }
	  }
	  break;

	default:
	  break;
	}
    }

  for (i = 0; decls->iterate (i, &idtx); ++i)
    dot_pass_dataFlowToplevel (idtx);

  dot_pass_dataFlow_popCtx ();
  return decls;
}
