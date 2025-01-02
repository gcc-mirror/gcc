/* Declarations for meta-data attribute tags.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Contributed by Iain Sandoe

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

/* These are identifiers used to flag meta-data attributes such that they
   survive LTO and might be placed in correct sections for the target.  */

enum objc_runtime_tree_index
{
  OCTI_RT_OBJC_META,
  OCTI_RT_META_BASE,

  OCTI_RT_META_CLASS,
  OCTI_RT_META_METACLASS,
  OCTI_RT_META_CATEGORY,
  OCTI_RT_META_PROTOCOL,

  OCTI_RT_META_CLASS_CLS_VARS,
  OCTI_RT_META_CLASS_NST_VARS,

  OCTI_RT_META_CLASS_CLS_METH,
  OCTI_RT_META_CLASS_NST_METH,
  OCTI_RT_META_CATEG_CLS_METH,
  OCTI_RT_META_CATEG_NST_METH,
  OCTI_RT_META_PROTO_CLS_METH,
  OCTI_RT_META_PROTO_NST_METH,

  OCTI_RT_META_CLASS_PROT,
  OCTI_RT_META_CATEG_PROT,
  OCTI_RT_META_PROT_REFS,

  OCTI_RT_META_MSG_REFS,
  OCTI_RT_META_SEL_REFS,

  OCTI_RT_META_CLSLST_REFS,
  OCTI_RT_META_CLASS_REF,
  OCTI_RT_META_SUPER_REF,
  OCTI_RT_META_CLSLST_NLZY_LAB,
  OCTI_RT_META_CLSLST_LAB,
  OCTI_RT_META_LAB_PROTOLIST,
  OCTI_RT_META_LAB_NLZY_CAT,
  OCTI_RT_META_LAB_CAT,

  OCTI_RT_META_PROPERTY_LIST,
  OCTI_RT_META_PROTOCOL_EXT,
  OCTI_RT_META_CLASS_EXT,

  OCTI_RT_META_CLASS_NAME,
  OCTI_RT_META_METHD_NAME,
  OCTI_RT_META_METHD_TYPE,
  OCTI_RT_META_PROPN_ATTR,

  OCTI_RT_META_MODULES,
  OCTI_RT_META_SYMTAB,
  OCTI_RT_META_INFO,

  OCTI_RT_META_EHTYPE,

  OCTI_RT_META_CONST_STR,

  OCTI_RT_META_IVAR_REF,

  OCTI_RT_META_MAX
};

extern GTY(()) tree objc_rt_trees[OCTI_RT_META_MAX];

/* Tags for the META data so that the backend can put them in the correct
   sections for targets/runtimes (Darwin/NeXT) that require this.
   This information also survives LTO - which might produce mixed language
   output.  */

/* Objective-C meta data attribute tag */
#define objc_meta	objc_rt_trees[OCTI_RT_OBJC_META]
/* Attribute values, base = default section. */
#define meta_base	objc_rt_trees[OCTI_RT_META_BASE]

  /* CLASS.  */
#define meta_class	objc_rt_trees[OCTI_RT_META_CLASS]
 /* METACLASS.  */
#define meta_metaclass	objc_rt_trees[OCTI_RT_META_METACLASS]
  /* CLASS.  */
#define meta_category	objc_rt_trees[OCTI_RT_META_CATEGORY]
  /* PROTOCOL.  */
#define meta_protocol	objc_rt_trees[OCTI_RT_META_PROTOCOL]

 /* Class class vars section.  */
#define meta_clac_vars	objc_rt_trees[OCTI_RT_META_CLASS_CLS_VARS]
 /* Class instance vars section.  */
#define meta_clai_vars	objc_rt_trees[OCTI_RT_META_CLASS_NST_VARS]
 /* Class class methods section.  */
#define meta_clac_meth	objc_rt_trees[OCTI_RT_META_CLASS_CLS_METH]
 /* Class instance methods section.  */
#define meta_clai_meth	objc_rt_trees[OCTI_RT_META_CLASS_NST_METH]
 /* Category class methods section.  */
#define meta_catc_meth	objc_rt_trees[OCTI_RT_META_CATEG_CLS_METH]
 /* Category instance methods section.  */
#define meta_cati_meth	objc_rt_trees[OCTI_RT_META_CATEG_NST_METH]
#define meta_proto_cls_meth \
			objc_rt_trees[OCTI_RT_META_PROTO_CLS_METH]
#define meta_proto_nst_meth \
			objc_rt_trees[OCTI_RT_META_PROTO_NST_METH]

 /* Class protocols.  */
#define meta_clas_prot	objc_rt_trees[OCTI_RT_META_CLASS_PROT]
 /* Category protocols.  */
#define meta_catg_prot	objc_rt_trees[OCTI_RT_META_CATEG_PROT]
 /* Protocol references.  */
#define meta_proto_ref	objc_rt_trees[OCTI_RT_META_PROT_REFS]

 /* Message refs.  */
#define meta_mref	objc_rt_trees[OCTI_RT_META_MSG_REFS]
 /* Selector refs.  */
#define meta_sel_refs	objc_rt_trees[OCTI_RT_META_SEL_REFS]

 /* Class list refs.  */
#define meta_class_ref	objc_rt_trees[OCTI_RT_META_CLSLST_REFS]
#define meta_class_reference \
			objc_rt_trees[OCTI_RT_META_CLASS_REF]
#define meta_superclass_ref \
			objc_rt_trees[OCTI_RT_META_SUPER_REF]
 /* Class list Label.  */
#define meta_label_classlist \
			objc_rt_trees[OCTI_RT_META_CLSLST_LAB]
 /* Class list Label (non lazy).  */
#define meta_label_nonlazy_classlist \
			objc_rt_trees[OCTI_RT_META_CLSLST_NLZY_LAB]
#define meta_label_categorylist \
			objc_rt_trees[OCTI_RT_META_LAB_CAT]
#define meta_label_nonlazy_categorylist \
			objc_rt_trees[OCTI_RT_META_LAB_NLZY_CAT]

#define meta_label_protocollist \
			objc_rt_trees[OCTI_RT_META_LAB_PROTOLIST]


/* V1 - property list.  */
#define meta_proplist	objc_rt_trees[OCTI_RT_META_PROPERTY_LIST]
#define meta_protocol_extension \
			objc_rt_trees[OCTI_RT_META_PROTOCOL_EXT]
#define meta_class_extension \
			objc_rt_trees[OCTI_RT_META_CLASS_EXT]
 /* String sections.  */
#define meta_class_name	objc_rt_trees[OCTI_RT_META_CLASS_NAME]
#define meta_meth_name	objc_rt_trees[OCTI_RT_META_METHD_NAME]
#define meta_meth_type	objc_rt_trees[OCTI_RT_META_METHD_TYPE]
#define meta_prop_name_attr \
			objc_rt_trees[OCTI_RT_META_PROPN_ATTR]

#define meta_modules	objc_rt_trees[OCTI_RT_META_MODULES]
#define meta_symtab	objc_rt_trees[OCTI_RT_META_SYMTAB]
#define meta_info	objc_rt_trees[OCTI_RT_META_INFO]

#define meta_ehtype	objc_rt_trees[OCTI_RT_META_EHTYPE]

#define meta_const_str	objc_rt_trees[OCTI_RT_META_CONST_STR]

#define meta_ivar_ref	objc_rt_trees[OCTI_RT_META_IVAR_REF]

#define OBJCMETA(DECL,VERS,KIND)					\
  if (VERS)								\
    DECL_ATTRIBUTES (DECL) = build_tree_list ((VERS), (KIND));
