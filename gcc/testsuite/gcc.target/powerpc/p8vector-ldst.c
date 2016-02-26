/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mupper-regs-df -mupper-regs-sf" } */

float
load_store_sf (unsigned long num,
	       const float *from_ptr,
	       float *to_ptr,
	       const unsigned long *in_mask_ptr,
	       const unsigned long *out_mask_ptr)
{
  float value00	= 0.0f;
  float value01	= 0.0f;
  float value02	= 0.0f;
  float value03	= 0.0f;
  float value04	= 0.0f;
  float value05	= 0.0f;
  float value06	= 0.0f;
  float value07	= 0.0f;
  float value08	= 0.0f;
  float value09	= 0.0f;
  float value10	= 0.0f;
  float value11	= 0.0f;
  float value12	= 0.0f;
  float value13	= 0.0f;
  float value14	= 0.0f;
  float value15	= 0.0f;
  float value16	= 0.0f;
  float value17	= 0.0f;
  float value18	= 0.0f;
  float value19	= 0.0f;
  float value20	= 0.0f;
  float value21	= 0.0f;
  float value22	= 0.0f;
  float value23	= 0.0f;
  float value24	= 0.0f;
  float value25	= 0.0f;
  float value26	= 0.0f;
  float value27	= 0.0f;
  float value28	= 0.0f;
  float value29	= 0.0f;
  float value30	= 0.0f;
  float value31	= 0.0f;
  float value32	= 0.0f;
  float value33	= 0.0f;
  float value34	= 0.0f;
  float value35	= 0.0f;
  float value36	= 0.0f;
  float value37	= 0.0f;
  float value38	= 0.0f;
  float value39	= 0.0f;
  unsigned long in_mask, in_mask2;
  unsigned long out_mask, out_mask2;
  unsigned long i;

  for (i = 0; i < num; i++)
    {
      in_mask = *in_mask_ptr++;
      in_mask2 = *in_mask_ptr++;
      if ((in_mask & (1L <<  0)) != 0L)
	value00 = *from_ptr++;

      if ((in_mask & (1L <<  1)) != 0L)
	value01 = *from_ptr++;

      if ((in_mask & (1L <<  2)) != 0L)
	value02 = *from_ptr++;

      if ((in_mask & (1L <<  3)) != 0L)
	value03 = *from_ptr++;

      if ((in_mask & (1L <<  4)) != 0L)
	value04 = *from_ptr++;

      if ((in_mask & (1L <<  5)) != 0L)
	value05 = *from_ptr++;

      if ((in_mask & (1L <<  6)) != 0L)
	value06 = *from_ptr++;

      if ((in_mask & (1L <<  7)) != 0L)
	value07 = *from_ptr++;

      if ((in_mask & (1L <<  8)) != 0L)
	value08 = *from_ptr++;

      if ((in_mask & (1L <<  9)) != 0L)
	value09 = *from_ptr++;

      if ((in_mask & (1L << 10)) != 0L)
	value10 = *from_ptr++;

      if ((in_mask & (1L << 11)) != 0L)
	value11 = *from_ptr++;

      if ((in_mask & (1L << 12)) != 0L)
	value12 = *from_ptr++;

      if ((in_mask & (1L << 13)) != 0L)
	value13 = *from_ptr++;

      if ((in_mask & (1L << 14)) != 0L)
	value14 = *from_ptr++;

      if ((in_mask & (1L << 15)) != 0L)
	value15 = *from_ptr++;

      if ((in_mask & (1L << 16)) != 0L)
	value16 = *from_ptr++;

      if ((in_mask & (1L << 17)) != 0L)
	value17 = *from_ptr++;

      if ((in_mask & (1L << 18)) != 0L)
	value18 = *from_ptr++;

      if ((in_mask & (1L << 19)) != 0L)
	value19 = *from_ptr++;

      if ((in_mask2 & (1L << 0)) != 0L)
	value20 = *from_ptr++;

      if ((in_mask2 & (1L << 1)) != 0L)
	value21 = *from_ptr++;

      if ((in_mask2 & (1L << 2)) != 0L)
	value22 = *from_ptr++;

      if ((in_mask2 & (1L << 3)) != 0L)
	value23 = *from_ptr++;

      if ((in_mask2 & (1L << 4)) != 0L)
	value24 = *from_ptr++;

      if ((in_mask2 & (1L << 5)) != 0L)
	value25 = *from_ptr++;

      if ((in_mask2 & (1L << 6)) != 0L)
	value26 = *from_ptr++;

      if ((in_mask2 & (1L << 7)) != 0L)
	value27 = *from_ptr++;

      if ((in_mask2 & (1L << 8)) != 0L)
	value28 = *from_ptr++;

      if ((in_mask2 & (1L << 9)) != 0L)
	value29 = *from_ptr++;

      if ((in_mask2 & (1L << 10)) != 0L)
	value30 = *from_ptr++;

      if ((in_mask2 & (1L << 11)) != 0L)
	value31 = *from_ptr++;

      if ((in_mask2 & (1L << 12)) != 0L)
	value32 = *from_ptr++;

      if ((in_mask2 & (1L << 13)) != 0L)
	value33 = *from_ptr++;

      if ((in_mask2 & (1L << 14)) != 0L)
	value34 = *from_ptr++;

      if ((in_mask2 & (1L << 15)) != 0L)
	value35 = *from_ptr++;

      if ((in_mask2 & (1L << 16)) != 0L)
	value36 = *from_ptr++;

      if ((in_mask2 & (1L << 17)) != 0L)
	value37 = *from_ptr++;

      if ((in_mask2 & (1L << 18)) != 0L)
	value38 = *from_ptr++;

      if ((in_mask2 & (1L << 19)) != 0L)
	value39 = *from_ptr++;

      out_mask = *out_mask_ptr++;
      out_mask2 = *out_mask_ptr++;
      if ((out_mask & (1L <<  0)) != 0L)
	*to_ptr++ = value00;

      if ((out_mask & (1L <<  1)) != 0L)
	*to_ptr++ = value01;

      if ((out_mask & (1L <<  2)) != 0L)
	*to_ptr++ = value02;

      if ((out_mask & (1L <<  3)) != 0L)
	*to_ptr++ = value03;

      if ((out_mask & (1L <<  4)) != 0L)
	*to_ptr++ = value04;

      if ((out_mask & (1L <<  5)) != 0L)
	*to_ptr++ = value05;

      if ((out_mask & (1L <<  6)) != 0L)
	*to_ptr++ = value06;

      if ((out_mask & (1L <<  7)) != 0L)
	*to_ptr++ = value07;

      if ((out_mask & (1L <<  8)) != 0L)
	*to_ptr++ = value08;

      if ((out_mask & (1L <<  9)) != 0L)
	*to_ptr++ = value09;

      if ((out_mask & (1L << 10)) != 0L)
	*to_ptr++ = value10;

      if ((out_mask & (1L << 11)) != 0L)
	*to_ptr++ = value11;

      if ((out_mask & (1L << 12)) != 0L)
	*to_ptr++ = value12;

      if ((out_mask & (1L << 13)) != 0L)
	*to_ptr++ = value13;

      if ((out_mask & (1L << 14)) != 0L)
	*to_ptr++ = value14;

      if ((out_mask & (1L << 15)) != 0L)
	*to_ptr++ = value15;

      if ((out_mask & (1L << 16)) != 0L)
	*to_ptr++ = value16;

      if ((out_mask & (1L << 17)) != 0L)
	*to_ptr++ = value17;

      if ((out_mask & (1L << 18)) != 0L)
	*to_ptr++ = value18;

      if ((out_mask & (1L << 19)) != 0L)
	*to_ptr++ = value19;

      if ((out_mask2 & (1L << 0)) != 0L)
	*to_ptr++ = value20;

      if ((out_mask2 & (1L << 1)) != 0L)
	*to_ptr++ = value21;

      if ((out_mask2 & (1L << 2)) != 0L)
	*to_ptr++ = value22;

      if ((out_mask2 & (1L << 3)) != 0L)
	*to_ptr++ = value23;

      if ((out_mask2 & (1L << 4)) != 0L)
	*to_ptr++ = value24;

      if ((out_mask2 & (1L << 5)) != 0L)
	*to_ptr++ = value25;

      if ((out_mask2 & (1L << 6)) != 0L)
	*to_ptr++ = value26;

      if ((out_mask2 & (1L << 7)) != 0L)
	*to_ptr++ = value27;

      if ((out_mask2 & (1L << 8)) != 0L)
	*to_ptr++ = value28;

      if ((out_mask2 & (1L << 9)) != 0L)
	*to_ptr++ = value29;

      if ((out_mask2 & (1L << 10)) != 0L)
	*to_ptr++ = value30;

      if ((out_mask2 & (1L << 11)) != 0L)
	*to_ptr++ = value31;

      if ((out_mask2 & (1L << 12)) != 0L)
	*to_ptr++ = value32;

      if ((out_mask2 & (1L << 13)) != 0L)
	*to_ptr++ = value33;

      if ((out_mask2 & (1L << 14)) != 0L)
	*to_ptr++ = value34;

      if ((out_mask2 & (1L << 15)) != 0L)
	*to_ptr++ = value35;

      if ((out_mask2 & (1L << 16)) != 0L)
	*to_ptr++ = value36;

      if ((out_mask2 & (1L << 17)) != 0L)
	*to_ptr++ = value37;

      if ((out_mask2 & (1L << 18)) != 0L)
	*to_ptr++ = value38;

      if ((out_mask2 & (1L << 19)) != 0L)
	*to_ptr++ = value39;
    }

  return (  value00 + value01 + value02 + value03 + value04
	  + value05 + value06 + value07 + value08 + value09
	  + value10 + value11 + value12 + value13 + value14
	  + value15 + value16 + value17 + value18 + value19
	  + value20 + value21 + value22 + value23 + value24
	  + value25 + value26 + value27 + value28 + value29
	  + value30 + value31 + value32 + value33 + value34
	  + value35 + value36 + value37 + value38 + value39);
}

double
load_store_df (unsigned long num,
	       const double *from_ptr,
	       double *to_ptr,
	       const unsigned long *in_mask_ptr,
	       const unsigned long *out_mask_ptr)
{
  double value00	= 0.0;
  double value01	= 0.0;
  double value02	= 0.0;
  double value03	= 0.0;
  double value04	= 0.0;
  double value05	= 0.0;
  double value06	= 0.0;
  double value07	= 0.0;
  double value08	= 0.0;
  double value09	= 0.0;
  double value10	= 0.0;
  double value11	= 0.0;
  double value12	= 0.0;
  double value13	= 0.0;
  double value14	= 0.0;
  double value15	= 0.0;
  double value16	= 0.0;
  double value17	= 0.0;
  double value18	= 0.0;
  double value19	= 0.0;
  double value20	= 0.0;
  double value21	= 0.0;
  double value22	= 0.0;
  double value23	= 0.0;
  double value24	= 0.0;
  double value25	= 0.0;
  double value26	= 0.0;
  double value27	= 0.0;
  double value28	= 0.0;
  double value29	= 0.0;
  double value30	= 0.0;
  double value31	= 0.0;
  double value32	= 0.0;
  double value33	= 0.0;
  double value34	= 0.0;
  double value35	= 0.0;
  double value36	= 0.0;
  double value37	= 0.0;
  double value38	= 0.0;
  double value39	= 0.0;
  unsigned long in_mask, in_mask2;
  unsigned long out_mask, out_mask2;
  unsigned long i;

  for (i = 0; i < num; i++)
    {
      in_mask = *in_mask_ptr++;
      in_mask2 = *in_mask_ptr++;
      if ((in_mask & (1L <<  0)) != 0L)
	value00 = *from_ptr++;

      if ((in_mask & (1L <<  1)) != 0L)
	value01 = *from_ptr++;

      if ((in_mask & (1L <<  2)) != 0L)
	value02 = *from_ptr++;

      if ((in_mask & (1L <<  3)) != 0L)
	value03 = *from_ptr++;

      if ((in_mask & (1L <<  4)) != 0L)
	value04 = *from_ptr++;

      if ((in_mask & (1L <<  5)) != 0L)
	value05 = *from_ptr++;

      if ((in_mask & (1L <<  6)) != 0L)
	value06 = *from_ptr++;

      if ((in_mask & (1L <<  7)) != 0L)
	value07 = *from_ptr++;

      if ((in_mask & (1L <<  8)) != 0L)
	value08 = *from_ptr++;

      if ((in_mask & (1L <<  9)) != 0L)
	value09 = *from_ptr++;

      if ((in_mask & (1L << 10)) != 0L)
	value10 = *from_ptr++;

      if ((in_mask & (1L << 11)) != 0L)
	value11 = *from_ptr++;

      if ((in_mask & (1L << 12)) != 0L)
	value12 = *from_ptr++;

      if ((in_mask & (1L << 13)) != 0L)
	value13 = *from_ptr++;

      if ((in_mask & (1L << 14)) != 0L)
	value14 = *from_ptr++;

      if ((in_mask & (1L << 15)) != 0L)
	value15 = *from_ptr++;

      if ((in_mask & (1L << 16)) != 0L)
	value16 = *from_ptr++;

      if ((in_mask & (1L << 17)) != 0L)
	value17 = *from_ptr++;

      if ((in_mask & (1L << 18)) != 0L)
	value18 = *from_ptr++;

      if ((in_mask & (1L << 19)) != 0L)
	value19 = *from_ptr++;

      if ((in_mask2 & (1L << 0)) != 0L)
	value20 = *from_ptr++;

      if ((in_mask2 & (1L << 1)) != 0L)
	value21 = *from_ptr++;

      if ((in_mask2 & (1L << 2)) != 0L)
	value22 = *from_ptr++;

      if ((in_mask2 & (1L << 3)) != 0L)
	value23 = *from_ptr++;

      if ((in_mask2 & (1L << 4)) != 0L)
	value24 = *from_ptr++;

      if ((in_mask2 & (1L << 5)) != 0L)
	value25 = *from_ptr++;

      if ((in_mask2 & (1L << 6)) != 0L)
	value26 = *from_ptr++;

      if ((in_mask2 & (1L << 7)) != 0L)
	value27 = *from_ptr++;

      if ((in_mask2 & (1L << 8)) != 0L)
	value28 = *from_ptr++;

      if ((in_mask2 & (1L << 9)) != 0L)
	value29 = *from_ptr++;

      if ((in_mask2 & (1L << 10)) != 0L)
	value30 = *from_ptr++;

      if ((in_mask2 & (1L << 11)) != 0L)
	value31 = *from_ptr++;

      if ((in_mask2 & (1L << 12)) != 0L)
	value32 = *from_ptr++;

      if ((in_mask2 & (1L << 13)) != 0L)
	value33 = *from_ptr++;

      if ((in_mask2 & (1L << 14)) != 0L)
	value34 = *from_ptr++;

      if ((in_mask2 & (1L << 15)) != 0L)
	value35 = *from_ptr++;

      if ((in_mask2 & (1L << 16)) != 0L)
	value36 = *from_ptr++;

      if ((in_mask2 & (1L << 17)) != 0L)
	value37 = *from_ptr++;

      if ((in_mask2 & (1L << 18)) != 0L)
	value38 = *from_ptr++;

      if ((in_mask2 & (1L << 19)) != 0L)
	value39 = *from_ptr++;

      out_mask = *out_mask_ptr++;
      out_mask2 = *out_mask_ptr++;
      if ((out_mask & (1L <<  0)) != 0L)
	*to_ptr++ = value00;

      if ((out_mask & (1L <<  1)) != 0L)
	*to_ptr++ = value01;

      if ((out_mask & (1L <<  2)) != 0L)
	*to_ptr++ = value02;

      if ((out_mask & (1L <<  3)) != 0L)
	*to_ptr++ = value03;

      if ((out_mask & (1L <<  4)) != 0L)
	*to_ptr++ = value04;

      if ((out_mask & (1L <<  5)) != 0L)
	*to_ptr++ = value05;

      if ((out_mask & (1L <<  6)) != 0L)
	*to_ptr++ = value06;

      if ((out_mask & (1L <<  7)) != 0L)
	*to_ptr++ = value07;

      if ((out_mask & (1L <<  8)) != 0L)
	*to_ptr++ = value08;

      if ((out_mask & (1L <<  9)) != 0L)
	*to_ptr++ = value09;

      if ((out_mask & (1L << 10)) != 0L)
	*to_ptr++ = value10;

      if ((out_mask & (1L << 11)) != 0L)
	*to_ptr++ = value11;

      if ((out_mask & (1L << 12)) != 0L)
	*to_ptr++ = value12;

      if ((out_mask & (1L << 13)) != 0L)
	*to_ptr++ = value13;

      if ((out_mask & (1L << 14)) != 0L)
	*to_ptr++ = value14;

      if ((out_mask & (1L << 15)) != 0L)
	*to_ptr++ = value15;

      if ((out_mask & (1L << 16)) != 0L)
	*to_ptr++ = value16;

      if ((out_mask & (1L << 17)) != 0L)
	*to_ptr++ = value17;

      if ((out_mask & (1L << 18)) != 0L)
	*to_ptr++ = value18;

      if ((out_mask & (1L << 19)) != 0L)
	*to_ptr++ = value19;

      if ((out_mask2 & (1L << 0)) != 0L)
	*to_ptr++ = value20;

      if ((out_mask2 & (1L << 1)) != 0L)
	*to_ptr++ = value21;

      if ((out_mask2 & (1L << 2)) != 0L)
	*to_ptr++ = value22;

      if ((out_mask2 & (1L << 3)) != 0L)
	*to_ptr++ = value23;

      if ((out_mask2 & (1L << 4)) != 0L)
	*to_ptr++ = value24;

      if ((out_mask2 & (1L << 5)) != 0L)
	*to_ptr++ = value25;

      if ((out_mask2 & (1L << 6)) != 0L)
	*to_ptr++ = value26;

      if ((out_mask2 & (1L << 7)) != 0L)
	*to_ptr++ = value27;

      if ((out_mask2 & (1L << 8)) != 0L)
	*to_ptr++ = value28;

      if ((out_mask2 & (1L << 9)) != 0L)
	*to_ptr++ = value29;

      if ((out_mask2 & (1L << 10)) != 0L)
	*to_ptr++ = value30;

      if ((out_mask2 & (1L << 11)) != 0L)
	*to_ptr++ = value31;

      if ((out_mask2 & (1L << 12)) != 0L)
	*to_ptr++ = value32;

      if ((out_mask2 & (1L << 13)) != 0L)
	*to_ptr++ = value33;

      if ((out_mask2 & (1L << 14)) != 0L)
	*to_ptr++ = value34;

      if ((out_mask2 & (1L << 15)) != 0L)
	*to_ptr++ = value35;

      if ((out_mask2 & (1L << 16)) != 0L)
	*to_ptr++ = value36;

      if ((out_mask2 & (1L << 17)) != 0L)
	*to_ptr++ = value37;

      if ((out_mask2 & (1L << 18)) != 0L)
	*to_ptr++ = value38;

      if ((out_mask2 & (1L << 19)) != 0L)
	*to_ptr++ = value39;
    }

  return (  value00 + value01 + value02 + value03 + value04
	  + value05 + value06 + value07 + value08 + value09
	  + value10 + value11 + value12 + value13 + value14
	  + value15 + value16 + value17 + value18 + value19
	  + value20 + value21 + value22 + value23 + value24
	  + value25 + value26 + value27 + value28 + value29
	  + value30 + value31 + value32 + value33 + value34
	  + value35 + value36 + value37 + value38 + value39);
}

/* { dg-final { scan-assembler "lxsspx"  } } */
/* { dg-final { scan-assembler "lxsdx"   } } */
/* { dg-final { scan-assembler "stxsspx" } } */
/* { dg-final { scan-assembler "stxsdx"  } } */
/* { dg-final { scan-assembler "xsaddsp" } } */
/* { dg-final { scan-assembler "xsadddp" } } */
