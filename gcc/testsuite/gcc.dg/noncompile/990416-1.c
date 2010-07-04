extern void *memcpy (void *, const void *, __SIZE_TYPE__);
typedef int word_type;
   
static void
copy_reg (unsigned int reg, frame_state *udata,	/* { dg-error "unknown type name" } */
	  frame_state *target_udata)	/* { dg-error "unknown type name" } */
{  
  word_type *preg = ge_reg_addr (reg, udata, 0);
  word_type *ptreg = ge_reg_addr (reg, target_udata, 0);
   
  memcpy (ptreg, preg, __builtin_dwarf_reg_size (reg));
}
