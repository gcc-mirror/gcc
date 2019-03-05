/* brig-util.cc -- gccbrig utility functions
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#include <sstream>

#include "stdint.h"
#include "hsa-brig-format.h"
#include "brig-util.h"
#include "errors.h"
#include "diagnostic-core.h"
#include "print-tree.h"

bool
group_variable_offset_index::has_variable (const std::string &name) const
{
  varname_offset_table::const_iterator i = m_group_offsets.find (name);
  return i != m_group_offsets.end ();
}

/* Adds a new group segment variable.  */

void
group_variable_offset_index::add (const std::string &name, size_t size,
				  size_t alignment)
{
  size_t align_padding = m_next_group_offset % alignment == 0 ?
    0 : (alignment - m_next_group_offset % alignment);
  m_next_group_offset += align_padding;
  m_group_offsets[name] = m_next_group_offset;
  m_next_group_offset += size;
}

size_t
group_variable_offset_index::segment_offset (const std::string &name) const
{
  varname_offset_table::const_iterator i = m_group_offsets.find (name);
  gcc_assert (i != m_group_offsets.end ());
  return (*i).second;
}

/* Return true if operand number OPNUM of instruction with OPCODE is an output.
   False if it is an input.  Some code reused from Martin Jambor's gcc-hsa
   tree.  */

bool
gccbrig_hsa_opcode_op_output_p (BrigOpcode16_t opcode, int opnum)
{
  switch (opcode)
    {
    case BRIG_OPCODE_BR:
    case BRIG_OPCODE_SBR:
    case BRIG_OPCODE_CBR:
    case BRIG_OPCODE_ST:
    case BRIG_OPCODE_ATOMICNORET:
    case BRIG_OPCODE_SIGNALNORET:
    case BRIG_OPCODE_INITFBAR:
    case BRIG_OPCODE_JOINFBAR:
    case BRIG_OPCODE_WAITFBAR:
    case BRIG_OPCODE_ARRIVEFBAR:
    case BRIG_OPCODE_LEAVEFBAR:
    case BRIG_OPCODE_RELEASEFBAR:
    case BRIG_OPCODE_DEBUGTRAP:
      return false;
    default:
      return opnum == 0;
    }
}

unsigned
gccbrig_hsa_type_bit_size (BrigType16_t t)
{

  unsigned pack_type = t & ~BRIG_TYPE_BASE_MASK;

  if (pack_type == BRIG_TYPE_PACK_32)
    return 32;
  else if (pack_type == BRIG_TYPE_PACK_64)
    return 64;
  else if (pack_type == BRIG_TYPE_PACK_128)
    return 128;

  switch (t)
    {
    case BRIG_TYPE_NONE:
      return 0;

    case BRIG_TYPE_B1:
      return 1;

    case BRIG_TYPE_U8:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_B8:
      return 8;

    case BRIG_TYPE_U16:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_B16:
    case BRIG_TYPE_F16:
      return 16;

    case BRIG_TYPE_U32:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_B32:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_U8X4:
    case BRIG_TYPE_U16X2:
    case BRIG_TYPE_S8X4:
    case BRIG_TYPE_S16X2:
    case BRIG_TYPE_F16X2:
    case BRIG_TYPE_SIG32:
      return 32;

    case BRIG_TYPE_U64:
    case BRIG_TYPE_S64:
    case BRIG_TYPE_F64:
    case BRIG_TYPE_B64:
    case BRIG_TYPE_U8X8:
    case BRIG_TYPE_U16X4:
    case BRIG_TYPE_U32X2:
    case BRIG_TYPE_S8X8:
    case BRIG_TYPE_S16X4:
    case BRIG_TYPE_S32X2:
    case BRIG_TYPE_F16X4:
    case BRIG_TYPE_F32X2:
    case BRIG_TYPE_SIG64:
      return 64;

    case BRIG_TYPE_B128:
    case BRIG_TYPE_U8X16:
    case BRIG_TYPE_U16X8:
    case BRIG_TYPE_U32X4:
    case BRIG_TYPE_U64X2:
    case BRIG_TYPE_S8X16:
    case BRIG_TYPE_S16X8:
    case BRIG_TYPE_S32X4:
    case BRIG_TYPE_S64X2:
    case BRIG_TYPE_F16X8:
    case BRIG_TYPE_F32X4:
    case BRIG_TYPE_F64X2:
      return 128;

    default:
      printf ("HMM %d %x\n", t, t);
      gcc_unreachable ();
    }
}

/* gcc-hsa borrowed code ENDS.  */

uint64_t
gccbrig_to_uint64_t (const BrigUInt64 &brig_type)
{
  return (uint64_t (brig_type.hi) << 32) | uint64_t (brig_type.lo);
}

int
gccbrig_reg_size (const BrigOperandRegister *brig_reg)
{
  switch (brig_reg->regKind)
    {
    case BRIG_REGISTER_KIND_CONTROL:
      return 1;
    case BRIG_REGISTER_KIND_SINGLE:
      return 32;
    case BRIG_REGISTER_KIND_DOUBLE:
      return 64;
    case BRIG_REGISTER_KIND_QUAD:
      return 128;
    default:
      gcc_unreachable ();
      break;
    }
}

std::string
gccbrig_reg_name (const BrigOperandRegister *reg)
{
  std::ostringstream strstr;
  switch (reg->regKind)
    {
    case BRIG_REGISTER_KIND_CONTROL:
      strstr << 'c';
      break;
    case BRIG_REGISTER_KIND_SINGLE:
      strstr << 's';
      break;
    case BRIG_REGISTER_KIND_DOUBLE:
      strstr << 'd';
      break;
    case BRIG_REGISTER_KIND_QUAD:
      strstr << 'q';
      break;
    default:
      gcc_unreachable ();
      return "";
    }
  strstr << reg->regNum;
  return strstr.str ();
}

std::string
gccbrig_type_name (BrigType16_t type)
{
  switch (type)
    {
    case BRIG_TYPE_U8:
      return "u8";
    case BRIG_TYPE_U16:
      return "u16";
    case BRIG_TYPE_U32:
      return "u32";
    case BRIG_TYPE_U64:
      return "u64";
    case BRIG_TYPE_S8:
      return "s8";
    case BRIG_TYPE_S16:
      return "s16";
    case BRIG_TYPE_S32:
      return "s32";
    case BRIG_TYPE_S64:
      return "s64";
    default:
      gcc_unreachable ();
      break;
    }
}

std::string
gccbrig_segment_name (BrigSegment8_t segment)
{
  if (segment == BRIG_SEGMENT_GLOBAL)
    return "global";
  else if (segment == BRIG_SEGMENT_GROUP)
    return "group";
  else if (segment == BRIG_SEGMENT_PRIVATE)
    return "private";
  else
    gcc_unreachable ();
}

bool
gccbrig_is_float_type (BrigType16_t type)
{
  return (type == BRIG_TYPE_F32 || type == BRIG_TYPE_F64
	  || type == BRIG_TYPE_F16);
}

BrigType16_t
gccbrig_tree_type_to_hsa_type (tree tree_type)
{
  if (INTEGRAL_TYPE_P (tree_type))
    {
      if (TYPE_UNSIGNED (tree_type))
	{
	  switch (int_size_in_bytes (tree_type))
	    {
	    case 1:
	      return BRIG_TYPE_U8;
	    case 2:
	      return BRIG_TYPE_U16;
	    case 4:
	      return BRIG_TYPE_U32;
	    case 8:
	      return BRIG_TYPE_U64;
	    default:
	      break;
	    }
	}
      else
	{
	  switch (int_size_in_bytes (tree_type))
	    {
	    case 1:
	      return BRIG_TYPE_S8;
	    case 2:
	      return BRIG_TYPE_S16;
	    case 4:
	      return BRIG_TYPE_S32;
	    case 8:
	      return BRIG_TYPE_S64;
	    default:
	      break;
	    }
	}
    }
  else if (VECTOR_TYPE_P (tree_type))
    {
      tree element_type = TREE_TYPE (tree_type);
      size_t element_size = int_size_in_bytes (element_type) * 8;
      BrigType16_t brig_element_type;
      switch (element_size)
	{
	case 8:
	  brig_element_type
	    = TYPE_UNSIGNED (element_type) ? BRIG_TYPE_U8 : BRIG_TYPE_S8;
	  break;
	case 16:
	  brig_element_type
	    = TYPE_UNSIGNED (element_type) ? BRIG_TYPE_U16 : BRIG_TYPE_S16;
	  break;
	case 32:
	  brig_element_type
	    = TYPE_UNSIGNED (element_type) ? BRIG_TYPE_U32 : BRIG_TYPE_S32;
	  break;
	case 64:
	  brig_element_type
	    = TYPE_UNSIGNED (element_type) ? BRIG_TYPE_U64 : BRIG_TYPE_S64;
	  break;
	default:
	  gcc_unreachable ();
	}

      BrigType16_t pack_type;
      switch (int_size_in_bytes (tree_type) * 8)
	{
	case 32:
	  pack_type = BRIG_TYPE_PACK_32;
	  break;
	case 64:
	  pack_type = BRIG_TYPE_PACK_64;
	  break;
	case 128:
	  pack_type = BRIG_TYPE_PACK_128;
	  break;
	default:
	  gcc_unreachable ();
	}
      return brig_element_type | pack_type;
    }
  gcc_unreachable ();
}

/* Returns true in case the operation is a "bit level" operation,
   that is, not having operand type depending semantical differences.  */

bool
gccbrig_is_bit_operation (BrigOpcode16_t opcode)
{
  return opcode == BRIG_OPCODE_CMOV || opcode == BRIG_OPCODE_SHUFFLE
	 || opcode == BRIG_OPCODE_UNPACK || opcode == BRIG_OPCODE_UNPACKLO
	 || opcode == BRIG_OPCODE_UNPACKHI || opcode == BRIG_OPCODE_ST
	 || opcode == BRIG_OPCODE_PACK;
}

/* The program scope definition can be left external within the
   kernel binary which means it must be defined by the host via
   HSA runtime.  For these we have special treatment:
   Create additional pointer indirection when accessing the variable
   value from kernel code through a generated pointer
   __gccbrig_ptr_variable_name.  The pointer value then can be set either
   within the kernel binary (in case of a later linked in definition)
   or from the host.  */

bool
gccbrig_might_be_host_defined_var_p (const BrigDirectiveVariable *brigVar)
{
  bool is_definition = brigVar->modifier & BRIG_VARIABLE_DEFINITION;
  return (brigVar->segment == BRIG_SEGMENT_GLOBAL
	  || brigVar->segment == BRIG_SEGMENT_READONLY) && !is_definition
    && brigVar->linkage == BRIG_LINKAGE_PROGRAM
    && (brigVar->allocation == BRIG_ALLOCATION_PROGRAM
	|| brigVar->allocation == BRIG_ALLOCATION_AGENT);
}

/* Produce a GENERIC type for the given HSA/BRIG type.  Returns the element
   type in case of vector instructions.  */

tree
gccbrig_tree_type_for_hsa_type (BrigType16_t brig_type)
{
  tree tree_type = NULL_TREE;

  if (hsa_type_packed_p (brig_type))
    {
      /* The element type is encoded in the bottom 5 bits.  */
      BrigType16_t inner_brig_type = brig_type & BRIG_TYPE_BASE_MASK;

      unsigned full_size = gccbrig_hsa_type_bit_size (brig_type);

      if (inner_brig_type == BRIG_TYPE_F16)
	return build_vector_type (gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U16),
				  full_size / 16);

      tree inner_type = gccbrig_tree_type_for_hsa_type (inner_brig_type);

      unsigned inner_size = gccbrig_hsa_type_bit_size (inner_brig_type);
      unsigned nunits = full_size / inner_size;
      tree_type = build_vector_type (inner_type, nunits);
    }
  else
    {
      switch (brig_type)
	{
	case BRIG_TYPE_NONE:
	  tree_type = void_type_node;
	  break;
	case BRIG_TYPE_B1:
	  tree_type = boolean_type_node;
	  break;
	case BRIG_TYPE_S8:
	case BRIG_TYPE_S16:
	case BRIG_TYPE_S32:
	case BRIG_TYPE_S64:
	  /* Ensure a fixed width integer.  */
	  tree_type
	    = build_nonstandard_integer_type
	    (gccbrig_hsa_type_bit_size (brig_type), false);
	  break;
	case BRIG_TYPE_U8:
	  return unsigned_char_type_node;
	case BRIG_TYPE_U16:
	case BRIG_TYPE_U32:
	case BRIG_TYPE_U64:
	case BRIG_TYPE_B8: /* Handle bit vectors as unsigned ints.  */
	case BRIG_TYPE_B16:
	case BRIG_TYPE_B32:
	case BRIG_TYPE_B64:
	case BRIG_TYPE_B128:
	case BRIG_TYPE_SIG32: /* Handle signals as integers for now.  */
	case BRIG_TYPE_SIG64:
	  tree_type = build_nonstandard_integer_type
	    (gccbrig_hsa_type_bit_size (brig_type), true);
	  break;
	case BRIG_TYPE_F16:
	  tree_type = uint16_type_node;
	  break;
	case BRIG_TYPE_F32:
	  /* TODO: make sure that the alignment of the float are at least as
	     strict than mandated by HSA, and conform to IEEE (like mandated
	     by HSA).  */
	  tree_type = float_type_node;
	  break;
	case BRIG_TYPE_F64:
	  tree_type = double_type_node;
	  break;
	case BRIG_TYPE_SAMP:
	case BRIG_TYPE_ROIMG:
	case BRIG_TYPE_WOIMG:
	case BRIG_TYPE_RWIMG:
	  {
	    /* Handle images and samplers as target-specific blobs of data
	       that should be allocated earlier on from the runtime side.
	       Create a void* that should be initialized to point to the blobs
	       by the kernel launcher.  Images and samplers are accessed
	       via builtins that take void* as the reference.  TODO: who and
	       how these arrays should be initialized?  */
	    tree void_ptr = build_pointer_type (void_type_node);
	    return void_ptr;
	  }
	default:
	  gcc_unreachable ();
	  break;
	}
    }

  /* Drop const qualifiers.  */
  return tree_type;
}

/* Calculates numeric identifier for the HSA register REG.

   Returned value is bound to [0, BRIG_2_TREE_HSAIL_TOTAL_REG_COUNT].  */

size_t
gccbrig_hsa_reg_id (const BrigOperandRegister &reg)
{
  size_t offset = reg.regNum;
  switch (reg.regKind)
    {
    case BRIG_REGISTER_KIND_QUAD:
      offset
	+= BRIG_2_TREE_HSAIL_D_REG_COUNT + BRIG_2_TREE_HSAIL_S_REG_COUNT
	+ BRIG_2_TREE_HSAIL_C_REG_COUNT;
      break;
    case BRIG_REGISTER_KIND_DOUBLE:
      offset += BRIG_2_TREE_HSAIL_S_REG_COUNT + BRIG_2_TREE_HSAIL_C_REG_COUNT;
      break;
    case BRIG_REGISTER_KIND_SINGLE:
      offset += BRIG_2_TREE_HSAIL_C_REG_COUNT;
    case BRIG_REGISTER_KIND_CONTROL:
      break;
    default:
      gcc_unreachable ();
      break;
    }
  return offset;
}

std::string
gccbrig_hsa_reg_name_from_id (size_t reg_id)
{
  char reg_name[32];
  long unsigned int reg_hash = (long unsigned int) reg_id;
  if (reg_hash < BRIG_2_TREE_HSAIL_C_REG_COUNT)
    {
      sprintf (reg_name, "$c%lu", reg_hash);
      return reg_name;
    }

  reg_hash -= BRIG_2_TREE_HSAIL_C_REG_COUNT;
  if (reg_hash < BRIG_2_TREE_HSAIL_S_REG_COUNT)
    {
      sprintf (reg_name, "$s%lu", reg_hash);
      return reg_name;
    }

  reg_hash -= BRIG_2_TREE_HSAIL_S_REG_COUNT;
  if (reg_hash < BRIG_2_TREE_HSAIL_D_REG_COUNT)
    {
      sprintf (reg_name, "$d%lu", reg_hash);
      return reg_name;
    }

   reg_hash -= BRIG_2_TREE_HSAIL_D_REG_COUNT;
   if (reg_hash < BRIG_2_TREE_HSAIL_Q_REG_COUNT)
    {
      sprintf (reg_name, "$q%lu", reg_hash);
      return reg_name;
    }

  gcc_unreachable ();
  return "$??";
}

/* Prints statistics of register usage to stdout.  */

void
gccbrig_print_reg_use_info (FILE *dump, const regs_use_index &info)
{
  regs_use_index::const_iterator begin_it = info.begin ();
  regs_use_index::const_iterator end_it = info.end ();
  for (regs_use_index::const_iterator it = begin_it; it != end_it; it++)
    {
      std::string hsa_reg = gccbrig_hsa_reg_name_from_id (it->first);
      printf ("%s:\n", hsa_reg.c_str ());
      const reg_use_info &info = it->second;
      typedef std::vector<std::pair<tree, size_t> >::const_iterator reg_use_it;
      reg_use_it begin_it2 = info.m_type_refs.begin ();
      reg_use_it end_it2 = info.m_type_refs.end ();
      for (reg_use_it it2 = begin_it2; it2 != end_it2; it2++)
	{
	  fprintf (dump, "(%lu) ", (long unsigned int) it2->second);
	  print_node_brief (dump, "", it2->first, 0);
	  fprintf (dump, "\n");
	}
    }
}
