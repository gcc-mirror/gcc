/* Definitions for branch prediction routines in the GNU compiler.
   Copyright (C) 2001, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#define DEF_PREDICTOR(ENUM, NAME, HITRATE, FLAGS) ENUM,
enum br_predictor
{
#include "predict.def"

  /* Upper bound on non-language-specific builtins.  */
  END_PREDICTORS
};
#undef DEF_PREDICTOR
enum prediction
{
   NOT_TAKEN,
   TAKEN
};

/* Flags for NOTE_PREDICTION */
#define IS_TAKEN 1		/* Predict edges to the block as taken.  */

extern void predict_insn_def (rtx, enum br_predictor, enum prediction);
extern void predict_insn (rtx, enum br_predictor, int);

/* Avoid unneeded dependency on basic_block.h.  */
#ifdef BASIC_BLOCK
extern void predict_edge (edge, enum br_predictor, int);
extern void predict_edge_def (edge, enum br_predictor, enum prediction);
#endif
