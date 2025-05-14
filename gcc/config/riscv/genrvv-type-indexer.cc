/* Generate the RVV type indexer tables.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#include "bconfig.h"
#define INCLUDE_SSTREAM
#include "system.h"
#include "errors.h"

#include "coretypes.h"

#include <assert.h>
#include <math.h>

#define BOOL_SIZE_LIST                                                         \
  {                                                                            \
    1, 2, 4, 8, 16, 32, 64                                                     \
  }
#define EEW_SIZE_LIST                                                          \
  {                                                                            \
    8, 16, 32, 64                                                              \
  }
#define LMUL1_LOG2 0

std::string
to_lmul (int lmul_log2)
{
  std::stringstream lmul_str;
  if (lmul_log2 >= 0)
    lmul_str << "m";
  else
    {
      lmul_str << "mf";
      lmul_log2 = -lmul_log2;
    }

  lmul_str << (1 << lmul_log2);
  return lmul_str.str ();
}

bool
valid_type (unsigned sew, int lmul_log2, bool float_p)
{
  if (lmul_log2 > 3)
    return false;

  switch (sew)
    {
    case 8:
      return lmul_log2 >= -3 && !float_p;
    case 16:
      return lmul_log2 >= -2;
    case 32:
      return lmul_log2 >= -1;
    case 64:
      return lmul_log2 >= 0;
    default:
      return false;
    }
}

bool
valid_type (unsigned sew, int lmul_log2, unsigned nf, bool float_p)
{
  if (!valid_type (sew, lmul_log2, float_p))
    return false;

  if (nf > 8 || nf < 1)
    return false;

  switch (lmul_log2)
    {
    case 1:
      return nf < 5;
    case 2:
      return nf < 3;
    case 3:
      return nf == 1;
    default:
      return true;
    }
}

std::string
inttype (unsigned sew, int lmul_log2, bool unsigned_p)
{
  if (!valid_type (sew, lmul_log2, /*float_t*/ false))
    return "INVALID";

  std::stringstream mode;
  mode << "v";
  if (unsigned_p)
    mode << "u";
  mode << "int" << sew << to_lmul (lmul_log2) << "_t";
  return mode.str ();
}

std::string
inttype (unsigned sew, int lmul_log2, unsigned nf, bool unsigned_p)
{
  if (!valid_type (sew, lmul_log2, nf, /*float_t*/ false))
    return "INVALID";

  std::stringstream mode;
  mode << "v";
  if (unsigned_p)
    mode << "u";
  mode << "int" << sew << to_lmul (lmul_log2);
  if (nf > 1)
    mode << "x" << nf;
  mode << "_t";
  return mode.str ();
}

std::string
bfloat16_type (int lmul_log2)
{
  if (!valid_type (16, lmul_log2, /*float_t*/ true))
    return "INVALID";

  std::stringstream mode;
  mode << "vbfloat16" << to_lmul (lmul_log2) << "_t";
  return mode.str ();
}

std::string
bfloat16_wide_type (int lmul_log2)
{
  if (!valid_type (32, lmul_log2, /*float_t*/ true))
    return "INVALID";

  std::stringstream mode;
  mode << "vfloat32" << to_lmul (lmul_log2) << "_t";
  return mode.str ();
}

std::string
bfloat16_type (int lmul_log2, unsigned nf)
{
  if (!valid_type (16, lmul_log2, nf, /*float_t*/ true))
    return "INVALID";

  std::stringstream mode;
  mode << "vbfloat16" << to_lmul (lmul_log2);
  if (nf > 1)
    mode << "x" << nf;
  mode << "_t";
  return mode.str ();
}

std::string
floattype (unsigned sew, int lmul_log2)
{
  if (!valid_type (sew, lmul_log2, /*float_t*/ true))
    return "INVALID";

  std::stringstream mode;
  mode << "vfloat" << sew << to_lmul (lmul_log2) << "_t";
  return mode.str ();
}

std::string
expand_floattype (unsigned sew, int lmul_log2, unsigned nf)
{
  if (sew != 8 || nf != 1
      || (!valid_type (sew * 4, lmul_log2 + 2, /*float_t*/ true)))
    return "INVALID";

  std::stringstream mode;
  mode << "vfloat" << sew * 4 << to_lmul (lmul_log2 + 2) << "_t";
  return mode.str ();
}

std::string
floattype (unsigned sew, int lmul_log2, unsigned nf)
{
  if (!valid_type (sew, lmul_log2, nf, /*float_t*/ true))
    return "INVALID";

  std::stringstream mode;
  mode << "vfloat" << sew << to_lmul (lmul_log2);
  if (nf > 1)
    mode << "x" << nf;
  mode << "_t";
  return mode.str ();
}

std::string
maskmode (unsigned sew, int lmul_log2)
{
  if (!valid_type (sew, lmul_log2, /*float_t*/ false))
    return "INVALID";

  std::stringstream mode;

  int mlen;
  if (lmul_log2 >= 0)
    mlen = sew / (1 << lmul_log2);
  else
    mlen = sew * (1 << -lmul_log2);

  mode << "vbool" << mlen << "_t";
  return mode.str ();
}

std::string
same_ratio_eew_type (unsigned sew, int lmul_log2, unsigned eew, bool unsigned_p,
		     bool float_p)
{
  if (!valid_type (sew, lmul_log2, float_p))
    return "INVALID";

  int elmul_log2;

  if (sew == eew)
    elmul_log2 = lmul_log2;
  else if (sew > eew)
    elmul_log2 = lmul_log2 - log2 (sew / eew);
  else /* sew < eew */
    elmul_log2 = lmul_log2 + log2 (eew / sew);

  if (float_p)
    return floattype (eew, elmul_log2);
  else
    return inttype (eew, elmul_log2, unsigned_p);
}

std::string
same_ratio_eew_bf16_type (unsigned sew, int lmul_log2)
{
  if (sew != 32)
    return "INVALID";
  int elmul_log2 = lmul_log2 - 1;
  return bfloat16_type (elmul_log2);
}

int
main (int argc, const char **argv)
{
  // Require at least one argument.
  if (argc < 2)
    return 1;

  FILE *fp = fopen (argv[1], "w");

  if (!fp)
    return 1;

  fprintf (fp, "/* Generated by genrvv-type-indexer */\n");

  for (unsigned vbool : {64, 32, 16, 8, 4, 2, 1})
    {
      std::stringstream mode;
      mode << "vbool" << vbool << "_t";
      fprintf (fp, "DEF_RVV_TYPE_INDEX (\n");
      fprintf (fp, "  /*VECTOR*/ %s,\n", mode.str ().c_str ());
      fprintf (fp, "  /*MASK*/ %s,\n", mode.str ().c_str ());
      fprintf (fp, "  /*SIGNED*/ INVALID,\n");
      fprintf (fp, "  /*UNSIGNED*/ INVALID,\n");
      fprintf (fp, "  /*SIGNED_EEW8_INDEX*/ INVALID,\n");
      for (unsigned eew : {8, 16, 32, 64})
	fprintf (fp, "  /*EEW%d_INDEX*/ INVALID,\n", eew);
      fprintf (fp, "  /*SHIFT*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_TRUNC*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_EMUL*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_EMUL_SIGNED*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_EMUL_UNSIGNED*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_FIX*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_FIX_SIGNED*/ INVALID,\n");
      fprintf (fp, "  /*QUAD_FIX_UNSIGNED*/ INVALID,\n");
      fprintf (fp, "  /*OCT_TRUNC*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_SCALAR*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_SIGNED*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED_SCALAR*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT_SCALAR*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT*/ INVALID,\n");
      fprintf (fp, "  /*DOUBLE_TRUNC_FLOAT*/ INVALID,\n");
      fprintf (fp, "  /*FLOAT*/ INVALID,\n");
      fprintf (fp, "  /*LMUL1*/ INVALID,\n");
      fprintf (fp, "  /*WLMUL1*/ INVALID,\n");
      fprintf (fp, "  /*QLMUL1*/ INVALID,\n");
      fprintf (fp, "  /*QLMUL1_SIGNED*/ INVALID,\n");
      fprintf (fp, "  /*QLMUL1_UNSIGNED*/ INVALID,\n");
      fprintf (fp, "  /*XFQF*/ INVALID,\n");
      for (unsigned eew : {8, 16, 32, 64})
	fprintf (fp, "  /*EEW%d_INTERPRET*/ INVALID,\n", eew);

      for (unsigned boolsize : BOOL_SIZE_LIST)
	fprintf (fp, "  /*BOOL%d_INTERPRET*/ INVALID,\n", boolsize);

      for (unsigned eew : EEW_SIZE_LIST)
	fprintf (fp, "  /*SIGNED_EEW%d_LMUL1_INTERPRET*/ %s,\n", eew,
		 inttype (eew, LMUL1_LOG2, /* unsigned_p */ false).c_str ());

      for (unsigned eew : EEW_SIZE_LIST)
	fprintf (fp, "  /*UNSIGNED_EEW%d_LMUL1_INTERPRET*/ %s,\n", eew,
		 inttype (eew, LMUL1_LOG2, /* unsigned_p */ true).c_str ());

      fprintf (fp, "  /*X2*/ INVALID,\n");

      for (unsigned lmul_log2_offset : {1, 2, 3, 4, 5, 6})
	{
	  unsigned multiple_of_lmul = 1 << lmul_log2_offset;
	  fprintf (fp, "  /*X%d_INTERPRET*/ INVALID,\n", multiple_of_lmul);
	}
      fprintf (fp, "  /*TUPLE_SUBPART*/ INVALID\n");
      fprintf (fp, ")\n");
    }

  // Build for vint and vuint
  for (unsigned sew : {8, 16, 32, 64})
    for (int lmul_log2 : {-3, -2, -1, 0, 1, 2, 3})
      for (unsigned nf : {1, 2, 3, 4, 5, 6, 7, 8})
	for (bool unsigned_p : {false, true})
	  {
	    if (!valid_type (sew, lmul_log2, nf, /*float_t*/ false))
	      continue;

	    fprintf (fp, "DEF_RVV_TYPE_INDEX (\n");
	    fprintf (fp, "  /*VECTOR*/ %s,\n",
		     inttype (sew, lmul_log2, nf, unsigned_p).c_str ());
	    fprintf (fp, "  /*MASK*/ %s,\n",
		     maskmode (sew, lmul_log2).c_str ());
	    fprintf (fp, "  /*SIGNED*/ %s,\n",
		     inttype (sew, lmul_log2, /*unsigned_p*/ false).c_str ());
	    fprintf (fp, "  /*UNSIGNED*/ %s,\n",
		     inttype (sew, lmul_log2, /*unsigned_p*/ true).c_str ());
	    fprintf (fp, "  /*SIGNED_EEW8_INDEX*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, 8,
					  /*unsigned_p*/ false, false)
		       .c_str ());
	    for (unsigned eew : {8, 16, 32, 64})
	      fprintf (fp, "  /*EEW%d_INDEX*/ %s,\n", eew,
		       same_ratio_eew_type (sew, lmul_log2, eew,
					    /*unsigned_p*/ true, false)
			 .c_str ());
	    fprintf (fp, "  /*SHIFT*/ %s,\n",
		     inttype (sew, lmul_log2, /*unsigned_p*/ true).c_str ());
	    fprintf (fp, "  /*DOUBLE_TRUNC*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 2, unsigned_p,
					  false)
		       .c_str ());
	    fprintf (fp, "  /*QUAD_TRUNC*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 4, unsigned_p,
					  false)
		       .c_str ());
	    fprintf (fp, "  /*QUAD_EMUL*/ %s,\n",
		     inttype (8, lmul_log2 - 1, unsigned_p).c_str ());
	    fprintf (fp, "  /*QUAD_EMUL_SIGNED*/ %s,\n",
		     inttype (8, lmul_log2 - 1, false).c_str ());
	    fprintf (fp, "  /*QUAD_EMUL_UNSIGNED*/ %s,\n",
		     inttype (8, lmul_log2 - 1, true).c_str ());
	    fprintf (fp, "  /*QUAD_FIX*/ %s,\n",
		     inttype (8, lmul_log2, unsigned_p).c_str ());
	    fprintf (fp, "  /*QUAD_FIX_SIGNED*/ %s,\n",
		     inttype (8, lmul_log2, false).c_str ());
	    fprintf (fp, "  /*QUAD_FIX_UNSIGNED*/ %s,\n",
		     inttype (8, lmul_log2, true).c_str ());
	    fprintf (fp, "  /*OCT_TRUNC*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 8, unsigned_p,
					  false)
		       .c_str ());
	    fprintf (fp, "  /*DOUBLE_TRUNC_SCALAR*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 2, unsigned_p,
					  false)
		       .c_str ());
	    fprintf (fp, "  /*DOUBLE_TRUNC_SIGNED*/ INVALID,\n");
	    fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 2, true, false)
		       .c_str ());
	    if (unsigned_p)
	      fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED_SCALAR*/ INVALID,\n");
	    else
	      fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED_SCALAR*/ %s,\n",
		       same_ratio_eew_type (sew, lmul_log2, sew / 2, true,
					    false)
			 .c_str ());
	    fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT_SCALAR*/ INVALID,\n");
	    fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT*/ INVALID,\n");
	    fprintf (fp, "  /*DOUBLE_TRUNC_FLOAT*/ %s,\n",
		     same_ratio_eew_type (sew, lmul_log2, sew / 2, false, true)
		       .c_str ());
	    fprintf (fp, "  /*FLOAT*/ %s,\n",
		     floattype (sew, lmul_log2).c_str ());
	    fprintf (fp, "  /*LMUL1*/ %s,\n",
		     inttype (sew, /*lmul_log2*/ 0, unsigned_p).c_str ());
	    fprintf (fp, "  /*WLMUL1*/ %s,\n",
		     inttype (sew * 2, /*lmul_log2*/ 0, unsigned_p).c_str ());
	    fprintf (fp, "  /*QLMUL1*/ %s,\n",
		     inttype (8, /*lmul_log2*/ 0, unsigned_p).c_str ());
	    fprintf (fp, "  /*QLMUL1_SIGNED*/ %s,\n",
		     inttype (8, /*lmul_log2*/ 0, false).c_str ());
	    fprintf (fp, "  /*QLMUL1_UNSIGNED*/ %s,\n",
		     inttype (8, /*lmul_log2*/ 0, true).c_str ());
	    fprintf (fp, "  /*XFQF*/ %s,\n",
		     expand_floattype (sew, lmul_log2, nf).c_str ());
	    for (unsigned eew : {8, 16, 32, 64})
	      {
		if (eew == sew)
		  fprintf (fp, "  /*EEW%d_INTERPRET*/ INVALID,\n", eew);
		else
		  fprintf (fp, "  /*EEW%d_INTERPRET*/ %s,\n", eew,
			   inttype (eew, lmul_log2, unsigned_p).c_str ());
	      }

	    for (unsigned boolsize : BOOL_SIZE_LIST)
	      {
		std::stringstream mode;
		mode << "vbool" << boolsize << "_t";

		fprintf (fp, "  /*BOOL%d_INTERPRET*/ %s,\n", boolsize,
			 nf == 1 && lmul_log2 == 0 ? mode.str ().c_str ()
						   : "INVALID");
	      }

	    for (unsigned eew : EEW_SIZE_LIST)
	      fprintf (fp, "  /*SIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n",
		       eew);

	    for (unsigned eew : EEW_SIZE_LIST)
	      fprintf (fp, "  /*UNSIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n",
		       eew);

	    fprintf (
	      fp, "  /*X2*/ %s,\n",
	      inttype (sew * 2, lmul_log2 + 1, /*unsigned_p*/ true).c_str ());

	    for (unsigned lmul_log2_offset : {1, 2, 3, 4, 5, 6})
	      {
		unsigned multiple_of_lmul = 1 << lmul_log2_offset;
		fprintf (fp, "  /*X%d_VLMUL_EXT*/ %s,\n", multiple_of_lmul,
			 inttype (sew, lmul_log2 + lmul_log2_offset, unsigned_p)
			   .c_str ());
	      }
	    fprintf (fp, "  /*TUPLE_SUBPART*/ %s\n",
		     inttype (sew, lmul_log2, 1, unsigned_p).c_str ());
	    fprintf (fp, ")\n");
	  }
  // Build for vbfloat16
  for (int lmul_log2 : {-2, -1, 0, 1, 2, 3})
    for (unsigned nf : {1, 2, 3, 4, 5, 6, 7, 8})
      {
	if (!valid_type (16, lmul_log2, nf, /*float_t*/ true))
	  continue;

	fprintf (fp, "DEF_RVV_TYPE_INDEX (\n");
	fprintf (fp, "  /*VECTOR*/ %s,\n",
		 bfloat16_type (lmul_log2, nf).c_str ());
	fprintf (fp, "  /*MASK*/ %s,\n", maskmode (16, lmul_log2).c_str ());
	fprintf (fp, "  /*SIGNED*/ %s,\n",
		 inttype (16, lmul_log2, /*unsigned_p*/ false).c_str ());
	fprintf (fp, "  /*UNSIGNED*/ %s,\n",
		 inttype (16, lmul_log2, /*unsigned_p*/ true).c_str ());
	fprintf (fp, "  /*SIGNED_EEW8_INDEX*/ INVALID,\n");
	for (unsigned eew : {8, 16, 32, 64})
	  fprintf (
	    fp, "  /*EEW%d_INDEX*/ %s,\n", eew,
	    same_ratio_eew_type (16, lmul_log2, eew, true, false).c_str ());
	fprintf (fp, "  /*SHIFT*/ INVALID,\n");
	fprintf (fp, "  /*DOUBLE_TRUNC*/ %s,\n",
		 same_ratio_eew_type (16, lmul_log2, 8, false, true).c_str ());
	fprintf (fp, "  /*QUAD_TRUNC*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_EMUL*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_EMUL_SIGNED*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_EMUL_UNSIGNED*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_FIX*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_FIX_SIGNED*/ INVALID,\n");
	fprintf (fp, "  /*QUAD_FIX_UNSIGNED*/ INVALID,\n");
	fprintf (fp, "  /*OCT_TRUNC*/ INVALID,\n");
	fprintf (fp, "  /*DOUBLE_TRUNC_SCALAR*/ %s,\n",
		 same_ratio_eew_type (16, lmul_log2, 8, false, true).c_str ());
	fprintf (fp, "  /*DOUBLE_TRUNC_SIGNED*/ %s,\n",
		 same_ratio_eew_type (16, lmul_log2, 8, false, false).c_str ());
	fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED*/ %s,\n",
		 same_ratio_eew_type (16, lmul_log2, 8, true, false).c_str ());
	fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED_SCALAR*/ INVALID,\n");
	fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT_SCALAR*/ INVALID,\n");
	fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT*/ INVALID,\n");
	fprintf (fp, "  /*DOUBLE_TRUNC_FLOAT*/ %s,\n",
		 same_ratio_eew_type (16, lmul_log2, 8, false, true).c_str ());
	fprintf (fp, "  /*FLOAT*/ INVALID,\n");
	fprintf (fp, "  /*LMUL1*/ %s,\n",
		 bfloat16_type (/*lmul_log2*/ 0).c_str ());
	fprintf (fp, "  /*WLMUL1*/ %s,\n",
		 bfloat16_wide_type (/*lmul_log2*/ 0).c_str ());
	fprintf (fp, "  /*QLMUL1*/ %s,\n",
		 bfloat16_wide_type (/*lmul_log2*/ 0).c_str ());
	fprintf (fp, "  /*QLMUL1_SIGNED*/ INVALID,\n");
	fprintf (fp, "  /*QLMUL1_UNSIGNED*/ INVALID,\n");
	fprintf (fp, "  /*XFQF*/ INVALID,\n");
	for (unsigned eew : {8, 16, 32, 64})
	  fprintf (fp, "  /*EEW%d_INTERPRET*/ INVALID,\n", eew);

	for (unsigned boolsize : BOOL_SIZE_LIST)
	  fprintf (fp, "  /*BOOL%d_INTERPRET*/ INVALID,\n", boolsize);

	for (unsigned eew : EEW_SIZE_LIST)
	  fprintf (fp, "  /*SIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n", eew);

	for (unsigned eew : EEW_SIZE_LIST)
	  fprintf (fp, "  /*UNSIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n", eew);

	fprintf (fp, "  /*X2*/ INVALID,\n");

	for (unsigned lmul_log2_offset : {1, 2, 3, 4, 5, 6})
	  {
	    unsigned multiple_of_lmul = 1 << lmul_log2_offset;
	    fprintf (fp, "  /*X%d_VLMUL_EXT*/ %s,\n", multiple_of_lmul,
		     bfloat16_type (lmul_log2 + lmul_log2_offset).c_str ());
	  }
	fprintf (fp, "  /*TUPLE_SUBPART*/ %s\n",
		 bfloat16_type (lmul_log2, 1U).c_str ());
	fprintf (fp, ")\n");
      }
  // Build for vfloat
  for (unsigned sew : {16, 32, 64})
    for (int lmul_log2 : {-3, -2, -1, 0, 1, 2, 3})
      for (unsigned nf : {1, 2, 3, 4, 5, 6, 7, 8})
	{
	  if (!valid_type (sew, lmul_log2, nf, /*float_t*/ true))
	    continue;

	  fprintf (fp, "DEF_RVV_TYPE_INDEX (\n");
	  fprintf (fp, "  /*VECTOR*/ %s,\n",
		   floattype (sew, lmul_log2, nf).c_str ());
	  fprintf (fp, "  /*MASK*/ %s,\n", maskmode (sew, lmul_log2).c_str ());
	  fprintf (fp, "  /*SIGNED*/ %s,\n",
		   inttype (sew, lmul_log2, /*unsigned_p*/ false).c_str ());
	  fprintf (fp, "  /*UNSIGNED*/ %s,\n",
		   inttype (sew, lmul_log2, /*unsigned_p*/ true).c_str ());
	  fprintf (fp, "  /*SIGNED_EEW8_INDEX*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, 8,
					/*unsigned_p*/ false, false)
		     .c_str ());
	  for (unsigned eew : {8, 16, 32, 64})
	    fprintf (fp, "  /*EEW%d_INDEX*/ %s,\n", eew,
		     same_ratio_eew_type (sew, lmul_log2, eew,
					  /*unsigned_p*/ true, false)
		       .c_str ());
	  fprintf (fp, "  /*SHIFT*/ INVALID,\n");
	  fprintf (fp, "  /*DOUBLE_TRUNC*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, sew / 2, false, true)
		     .c_str ());
	  fprintf (fp, "  /*QUAD_TRUNC*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_EMUL*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_EMUL_SIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_EMUL_UNSIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_FIX*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_FIX_SIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*QUAD_FIX_UNSIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*OCT_TRUNC*/ INVALID,\n");
	  fprintf (fp, "  /*DOUBLE_TRUNC_SCALAR*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, sew / 2, false, true)
		     .c_str ());
	  fprintf (fp, "  /*DOUBLE_TRUNC_SIGNED*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, sew / 2, false, false)
		     .c_str ());
	  fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, sew / 2, true, false)
		     .c_str ());
	  fprintf (fp, "  /*DOUBLE_TRUNC_UNSIGNED_SCALAR*/ INVALID,\n");
	  fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT_SCALAR*/ %s,\n",
		   same_ratio_eew_bf16_type (sew, lmul_log2).c_str ());
	  fprintf (fp, "  /*DOUBLE_TRUNC_BFLOAT*/ %s,\n",
		   same_ratio_eew_bf16_type (sew, lmul_log2).c_str ());
	  fprintf (fp, "  /*DOUBLE_TRUNC_FLOAT*/ %s,\n",
		   same_ratio_eew_type (sew, lmul_log2, sew / 2, false, true)
		     .c_str ());
	  fprintf (fp, "  /*FLOAT*/ INVALID,\n");
	  fprintf (fp, "  /*LMUL1*/ %s,\n",
		   floattype (sew, /*lmul_log2*/ 0).c_str ());
	  fprintf (fp, "  /*WLMUL1*/ %s,\n",
		   floattype (sew * 2, /*lmul_log2*/ 0).c_str ());
	  fprintf (fp, "  /*QLMUL1*/ %s,\n",
		   floattype (sew / 4, /*lmul_log2*/ 0).c_str ());
	  fprintf (fp, "  /*QLMUL1_SIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*QLMUL1_UNSIGNED*/ INVALID,\n");
	  fprintf (fp, "  /*XFQF*/ INVALID,\n");
	  for (unsigned eew : {8, 16, 32, 64})
	    fprintf (fp, "  /*EEW%d_INTERPRET*/ INVALID,\n", eew);

	  for (unsigned boolsize : BOOL_SIZE_LIST)
	    fprintf (fp, "  /*BOOL%d_INTERPRET*/ INVALID,\n", boolsize);

	  for (unsigned eew : EEW_SIZE_LIST)
	    fprintf (fp, "  /*SIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n", eew);

	  for (unsigned eew : EEW_SIZE_LIST)
	    fprintf (fp, "  /*UNSIGNED_EEW%d_LMUL1_INTERPRET*/ INVALID,\n",
		     eew);

	  fprintf (fp, "  /*X2*/ INVALID,\n");

	  for (unsigned lmul_log2_offset : {1, 2, 3, 4, 5, 6})
	    {
	      unsigned multiple_of_lmul = 1 << lmul_log2_offset;
	      fprintf (fp, "  /*X%d_VLMUL_EXT*/ %s,\n", multiple_of_lmul,
		       floattype (sew, lmul_log2 + lmul_log2_offset).c_str ());
	    }
	  fprintf (fp, "  /*TUPLE_SUBPART*/ %s\n",
		   floattype (sew, lmul_log2, 1).c_str ());
	  fprintf (fp, ")\n");
	}

  return 0;
}
