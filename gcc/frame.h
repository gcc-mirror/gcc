/* Copyright (C) 1997 Free Software Foundation, Inc.
   This file is part of GNU CC.  */

typedef struct frame_state
{
  void *cfa;
  void *eh_ptr;
  long cfa_offset;
  long args_size;
  long reg_or_offset[FIRST_PSEUDO_REGISTER+1];
  unsigned short cfa_reg;
  unsigned short retaddr_column;
  char saved[FIRST_PSEUDO_REGISTER+1];
} frame_state;

/* Values for 'saved' above.  */
#define REG_UNSAVED 0
#define REG_SAVED_OFFSET 1
#define REG_SAVED_REG 2

/* Called either from crtbegin.o or a static constructor to register the
   unwind info for an object or translation unit, respectively.  */

extern void __register_frame (void *);

/* Called from crtend.o to deregister the unwind info for an object.  */

extern void __deregister_frame (void *);

/* Called from __throw to find the registers to restore for a given
   PC_TARGET.  The caller should allocate a local variable of `struct
   frame_state' (declared in frame.h) and pass its address to STATE_IN.
   Returns NULL on failure, otherwise returns STATE_IN.  */

extern struct frame_state *__frame_state_for (void *, struct frame_state *);
