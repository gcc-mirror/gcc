/* { dg-do compile } */

float fres, f1, f2;
int ires, i1, i2;
void *pres, *p1, *p2;

void x ()
{
  __builtin_custom_n (0);
  __builtin_custom_ni (1, i1);
  __builtin_custom_nf (2, f1);
  __builtin_custom_np (3, p1);
  __builtin_custom_nii (4, i1, i2);
  __builtin_custom_nif (5, i1, f2);
  __builtin_custom_nip (6, i1, p2);
  __builtin_custom_nfi (7, f1, i2);
  __builtin_custom_nff (8, f1, f2);
  __builtin_custom_nfp (9, f1, p2);
  __builtin_custom_npi (10, p1, i2);
  __builtin_custom_npf (11, p1, f2);
  __builtin_custom_npp (12, p1, p2);

  ires = __builtin_custom_in (13+0);
  ires = __builtin_custom_ini (13+1, i1);
  ires = __builtin_custom_inf (13+2, f1);
  ires = __builtin_custom_inp (13+3, p1);
  ires = __builtin_custom_inii (13+4, i1, i2);
  ires = __builtin_custom_inif (13+5, i1, f2);
  ires = __builtin_custom_inip (13+6, i1, p2);
  ires = __builtin_custom_infi (13+7, f1, i2);
  ires = __builtin_custom_inff (13+8, f1, f2);
  ires = __builtin_custom_infp (13+9, f1, p2);
  ires = __builtin_custom_inpi (13+10, p1, i2);
  ires = __builtin_custom_inpf (13+11, p1, f2);
  ires = __builtin_custom_inpp (13+12, p1, p2);

  fres = __builtin_custom_fn (26+0);
  fres = __builtin_custom_fni (26+1, i1);
  fres = __builtin_custom_fnf (26+2, f1);
  fres = __builtin_custom_fnp (26+3, p1);
  fres = __builtin_custom_fnii (26+4, i1, i2);
  fres = __builtin_custom_fnif (26+5, i1, f2);
  fres = __builtin_custom_fnip (26+6, i1, p2);
  fres = __builtin_custom_fnfi (26+7, f1, i2);
  fres = __builtin_custom_fnff (26+8, f1, f2);
  fres = __builtin_custom_fnfp (26+9, f1, p2);
  fres = __builtin_custom_fnpi (26+10, p1, i2);
  fres = __builtin_custom_fnpf (26+11, p1, f2);
  fres = __builtin_custom_fnpp (26+12, p1, p2);

  pres = __builtin_custom_pn (39+0);
  pres = __builtin_custom_pni (39+1, i1);
  pres = __builtin_custom_pnf (39+2, f1);
  pres = __builtin_custom_pnp (39+3, p1);
  pres = __builtin_custom_pnii (39+4, i1, i2);
  pres = __builtin_custom_pnif (39+5, i1, f2);
  pres = __builtin_custom_pnip (39+6, i1, p2);
  pres = __builtin_custom_pnfi (39+7, f1, i2);
  pres = __builtin_custom_pnff (39+8, f1, f2);
  pres = __builtin_custom_pnfp (39+9, f1, p2);
  pres = __builtin_custom_pnpi (39+10, p1, i2);
  pres = __builtin_custom_pnpf (39+11, p1, f2);
  pres = __builtin_custom_pnpp (39+12, p1, p2);
} 
