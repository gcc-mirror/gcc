From gcc-patches-return-180556-listarch-gcc-patches=gcc dot gnu dot org at gcc dot gnu dot org Thu Oct 26 21:28:07 2006
Return-Path: <gcc-patches-return-180556-listarch-gcc-patches=gcc dot gnu dot org at gcc dot gnu dot org>
Delivered-To: listarch-gcc-patches at gcc dot gnu dot org
Received: (qmail 19726 invoked by alias); 26 Oct 2006 21:28:06 -0000
Received: (qmail 19713 invoked by uid 22791); 26 Oct 2006 21:28:05 -0000
X-Spam-Check-By: sourceware.org
Received: from nikam-dmz.ms.mff.cuni.cz (HELO nikam.ms.mff.cuni.cz) (195.113.20.16)     by sourceware.org (qpsmtpd/0.31) with ESMTP; Thu, 26 Oct 2006 21:28:02 +0000
Received: from occam.ms.mff.cuni.cz (occam.ms.mff.cuni.cz [195.113.18.121]) 	by nikam.ms.mff.cuni.cz (Postfix) with SMTP id F07CC5BA3F 	for <gcc-patches@gcc.gnu.org>; Thu, 26 Oct 2006 23:27:59 +0200 (CEST)
Received: by occam.ms.mff.cuni.cz (sSMTP sendmail emulation); Thu, 26 Oct 2006 23:27:59 +0200
Date: Thu, 26 Oct 2006 23:27:59 +0200
From: Jan Hubicka <jh at suse dot cz>
To: gcc-patches at gcc dot gnu dot org
Subject: More memcpy folding
Message-ID: <20061026212759.GD6035@kam.mff.cuni.cz>
Mime-Version: 1.0
Content-Type: text/plain; charset=us-ascii
Content-Disposition: inline
User-Agent: Mutt/1.5.9i
Mailing-List: contact gcc-patches-help at gcc dot gnu dot org; run by ezmlm
Precedence: bulk
List-Archive: <http://gcc.gnu.org/ml/gcc-patches/>
List-Post: <mailto:gcc-patches at gcc dot gnu dot org>
List-Help: <mailto:gcc-patches-help at gcc dot gnu dot org>
Sender: gcc-patches-owner at gcc dot gnu dot org
Delivered-To: mailing list gcc-patches at gcc dot gnu dot org

Hi,
this patch extends Jakub's code to fold memcpy into assignment to case of
structures to make GCC understand common low-level C idiom of
memcpy (&a, &b, sizeof (*a))
for copying structures.  This is of less academic interested as my previous
memmove folding since it matches quite few times during GCC bootstrap
and during SPEC build.  The transfomration often kills last place taking
address of the argument thus allowing SRA and other optimizations.

I tried to cover as many cases as possible.  Unforutnately there are
aliasing issues as shown by execute/20060930-2.c, so I need to check
var_decl_component_p.  With little help from PTA we should be probably
able to do better here.

Bootstrapped/regtested i686-linux, OK?
:ADDPATCH middle-end:

Honza

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "nasty_local" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
struct a {int a,b,c;} a;
int test(struct a a)
{
struct a nasty_local;
memcpy (&nasty_local,&a, sizeof(a));
return nasty_local.a;
}
	* builtins.c: Include tree-flow.h.
	(fold_builtin_memory_op): Be more aggressive on converting memcpy to
	assignment; convert memmove to memcpy for sizes greater than 1 where
	alignment of operands prohibit the partial overlap.
Index: builtins.c
===================================================================
*** builtins.c	(revision 118067)
--- builtins.c	(working copy)
*************** Software Foundation, 51 Franklin Street,
*** 47,52 ****
--- 47,53 ----
  #include "langhooks.h"
  #include "basic-block.h"
  #include "tree-mudflap.h"
+ #include "tree-flow.h"
  
  #ifndef PAD_VARARGS_DOWN
  #define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
*************** static tree
*** 8029,8035 ****
  fold_builtin_memory_op (tree arglist, tree type, bool ignore, int endp)
  {
    tree dest, src, len, destvar, srcvar, expr;
-   unsigned HOST_WIDE_INT length;
  
    if (! validate_arglist (arglist,
  			  POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
--- 8030,8035 ----
*************** fold_builtin_memory_op (tree arglist, tr
*** 8049,8060 ****
      expr = len;
    else
      {
        if (endp == 3)
  	{
!           unsigned int src_align
! 	     = get_pointer_alignment (src, BIGGEST_ALIGNMENT);
!           unsigned int dest_align
! 	     = get_pointer_alignment (dest, BIGGEST_ALIGNMENT);
  	  /* Both DEST and SRC must be pointer types. 
  	     ??? This is what old code did.  Is the testing for pointer types
  	     really mandatory?
--- 8049,8060 ----
      expr = len;
    else
      {
+       tree srctype, desttype;
        if (endp == 3)
  	{
!           int src_align = get_pointer_alignment (src, BIGGEST_ALIGNMENT);
!           int dest_align = get_pointer_alignment (dest, BIGGEST_ALIGNMENT);
! 
  	  /* Both DEST and SRC must be pointer types. 
  	     ??? This is what old code did.  Is the testing for pointer types
  	     really mandatory?
*************** fold_builtin_memory_op (tree arglist, tr
*** 8062,8125 ****
  	     If either SRC is readonly or length is 1, we can use memcpy.  */
  	  if (dest_align && src_align
  	      && (readonly_data_expr (src)
! 		  || integer_onep (len)))
  	    {
  	      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  	      if (!fn)
  		return 0;
  	      return build_function_call_expr (fn, arglist);
  	    }
  	}
-       if (! host_integerp (len, 1))
- 	return 0;
- 
-       if (TREE_SIDE_EFFECTS (dest) || TREE_SIDE_EFFECTS (src))
- 	return 0;
- 
-       destvar = dest;
-       STRIP_NOPS (destvar);
-       if (TREE_CODE (destvar) != ADDR_EXPR)
- 	return 0;
- 
-       destvar = TREE_OPERAND (destvar, 0);
-       if (TREE_THIS_VOLATILE (destvar))
- 	return 0;
  
!       if (!INTEGRAL_TYPE_P (TREE_TYPE (destvar))
! 	  && !POINTER_TYPE_P (TREE_TYPE (destvar))
! 	  && !SCALAR_FLOAT_TYPE_P (TREE_TYPE (destvar)))
  	return 0;
! 
!       if (! var_decl_component_p (destvar))
  	return 0;
  
!       srcvar = src;
!       STRIP_NOPS (srcvar);
!       if (TREE_CODE (srcvar) != ADDR_EXPR)
! 	return 0;
  
!       srcvar = TREE_OPERAND (srcvar, 0);
        if (TREE_THIS_VOLATILE (srcvar))
  	return 0;
! 
!       if (!INTEGRAL_TYPE_P (TREE_TYPE (srcvar))
! 	  && !POINTER_TYPE_P (TREE_TYPE (srcvar))
! 	  && !SCALAR_FLOAT_TYPE_P (TREE_TYPE (srcvar)))
  	return 0;
  
!       if (! var_decl_component_p (srcvar))
  	return 0;
! 
!       length = tree_low_cst (len, 1);
!       if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (destvar))) != length
! 	  || get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT
! 	     < (int) length
! 	  || GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (srcvar))) != length
! 	  || get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT
! 	     < (int) length)
  	return 0;
  
!       if ((INTEGRAL_TYPE_P (TREE_TYPE (srcvar))
  	   || POINTER_TYPE_P (TREE_TYPE (srcvar)))
  	  && (INTEGRAL_TYPE_P (TREE_TYPE (destvar))
  	      || POINTER_TYPE_P (TREE_TYPE (destvar))))
--- 8062,8133 ----
  	     If either SRC is readonly or length is 1, we can use memcpy.  */
  	  if (dest_align && src_align
  	      && (readonly_data_expr (src)
! 	          || (host_integerp (len, 1)
! 		      && (MIN (src_align, dest_align) / BITS_PER_UNIT <=
! 			  tree_low_cst (len, 1)))))
  	    {
  	      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  	      if (!fn)
  		return 0;
  	      return build_function_call_expr (fn, arglist);
  	    }
+ 	  return 0;
  	}
  
!       if (!host_integerp (len, 0))
  	return 0;
!       /* FIXME:
!          This logic lose for arguments like (type *)malloc (sizeof (type)),
!          since we strip the casts of up to VOID return value from malloc.
! 	 Perhaps we ought to inherit type from non-VOID argument here?  */
!       STRIP_NOPS (src);
!       STRIP_NOPS (dest);
!       srctype = TREE_TYPE (TREE_TYPE (src));
!       desttype = TREE_TYPE (TREE_TYPE (dest));
!       if (!srctype || !desttype
! 	  || !TYPE_SIZE_UNIT (srctype)
! 	  || !TYPE_SIZE_UNIT (desttype)
! 	  || TREE_CODE (TYPE_SIZE_UNIT (srctype)) != INTEGER_CST
! 	  || TREE_CODE (TYPE_SIZE_UNIT (desttype)) != INTEGER_CST
! 	  || !operand_equal_p (TYPE_SIZE_UNIT (srctype), len, 0)
! 	  || !operand_equal_p (TYPE_SIZE_UNIT (desttype), len, 0))
! 	return 0;
! 
!       if (get_pointer_alignment (dest, BIGGEST_ALIGNMENT) 
! 	  < (int) TYPE_ALIGN (desttype)
! 	  || (get_pointer_alignment (src, BIGGEST_ALIGNMENT) 
! 	      < (int) TYPE_ALIGN (srctype)))
  	return 0;
  
!       if (!ignore)
!         dest = builtin_save_expr (dest);
  
!       srcvar = build_fold_indirect_ref (src);
        if (TREE_THIS_VOLATILE (srcvar))
  	return 0;
!       /* With memcpy, it is possible to bypass aliasing rules, so without
!          this check i. e. execute/20060930-2.c would be misoptimized, because
! 	 it use conflicting alias set to hold argument for the memcpy call.
! 	 This check is probably unnecesary with -fno-strict-aliasing. 
! 	 Similarly for destvar.  See also PR29286.  */
!       if (!var_decl_component_p (srcvar)
! 	  /* Accept: memcpy (*char_var, "test", 1); that simplify
! 	     to char_var='t';  */
! 	  || is_gimple_min_invariant (srcvar)
! 	  || readonly_data_expr (src))
  	return 0;
  
!       destvar = build_fold_indirect_ref (dest);
!       if (TREE_THIS_VOLATILE (destvar))
  	return 0;
!       if (!var_decl_component_p (destvar))
  	return 0;
  
!       if (srctype == desttype
! 	  || (in_ssa_p
! 	      && tree_ssa_useless_type_conversion_1 (desttype, srctype)))
! 	expr = srcvar;
!       else if ((INTEGRAL_TYPE_P (TREE_TYPE (srcvar))
  	   || POINTER_TYPE_P (TREE_TYPE (srcvar)))
  	  && (INTEGRAL_TYPE_P (TREE_TYPE (destvar))
  	      || POINTER_TYPE_P (TREE_TYPE (destvar))))

