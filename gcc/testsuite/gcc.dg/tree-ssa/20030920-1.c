/* Jump threading was creating FALLTHRU edges out of blocks ending in
   GOTO_EXPR.  */

extern int frame_pointer_needed;

struct value_data_entry
{
  unsigned int mode;
  unsigned int oldest_regno;
  unsigned int next_regno;
};

struct value_data
{
  struct value_data_entry e[53];
  unsigned int max_value_regs;
};

struct rtx_def
{
  unsigned int code: 16;
  unsigned int mode : 8;
  unsigned int jump : 1;
  unsigned int call : 1;
  unsigned int unchanging : 1;
  unsigned int volatil : 1;
  unsigned int in_struct : 1;
  unsigned int used : 1;
  unsigned integrated : 1;
  unsigned frame_related : 1;
  int fld[1];
};

typedef struct rtx_def *rtx;

enum machine_mode { VOIDmode, BImode, QImode, HImode, SImode, DImode,
    TImode, OImode, PQImode, PHImode, PSImode, PDImode, QFmode, HFmode,
    TQFmode, SFmode, DFmode, XFmode, TFmode, QCmode, HCmode, SCmode,
    DCmode, XCmode, TCmode, CQImode, CHImode, CSImode, CDImode, CTImode,
    COImode, V1DImode, V2QImode, V2HImode, V2SImode, V2DImode, V4QImode,
    V4HImode, V4SImode, V4DImode, V8QImode, V8HImode, V8SImode, V8DImode,
    V16QImode, V2HFmode, V2SFmode, V2DFmode, V4HFmode, V4SFmode, V4DFmode,
    V8HFmode, V8SFmode, V8DFmode, V16SFmode, BLKmode, CCmode, CCGCmode,
    CCGOCmode, CCNOmode, CCZmode, CCFPmode, CCFPUmode, MAX_MACHINE_MODE };

enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
    MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT,
    MODE_VECTOR_INT, MODE_VECTOR_FLOAT,
    MAX_MODE_CLASS};

extern const unsigned char mode_size[(int) MAX_MACHINE_MODE];
extern const enum mode_class mode_class[(int) MAX_MACHINE_MODE];

extern int target_flags;

static void
copy_value (rtx dest, rtx src, struct value_data *vd)
{
  unsigned int dr = (((dest)->fld[0]));
  unsigned int sr = (((src)->fld[0]));
  unsigned int dn, sn;
  unsigned int i;



  if (sr == dr)
    return;



  if (dr == 7)
    return;


  if (frame_pointer_needed && dr == 6)
    return;


  dn = (((dr) >= 8 && (dr) <= (8 + 7)) || (((dr) >= (20 + 1) && (dr) <= ((20 + 1) + 7)) || ((dr) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (dr) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((dr) >= (((20 + 1) + 7) + 1) && (dr) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_INT || (mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((((enum machine_mode) (dest)->mode)) == TFmode ? ((target_flags & 0x00100000) ? 2 : 3) : (((enum machine_mode) (dest)->mode)) == TCmode ? ((target_flags & 0x00100000) ? 4 : 6) : (((mode_size[(int) (((enum machine_mode) (dest)->mode))]) + ((target_flags & 0x00100000) ? 8 : 4) - 1) / ((target_flags & 0x00100000) ? 8 : 4))));
  sn = (((sr) >= 8 && (sr) <= (8 + 7)) || (((sr) >= (20 + 1) && (sr) <= ((20 + 1) + 7)) || ((sr) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (sr) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((sr) >= (((20 + 1) + 7) + 1) && (sr) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_INT || (mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((((enum machine_mode) (dest)->mode)) == TFmode ? ((target_flags & 0x00100000) ? 2 : 3) : (((enum machine_mode) (dest)->mode)) == TCmode ? ((target_flags & 0x00100000) ? 4 : 6) : (((mode_size[(int) (((enum machine_mode) (dest)->mode))]) + ((target_flags & 0x00100000) ? 8 : 4) - 1) / ((target_flags & 0x00100000) ? 8 : 4))));
  if ((dr > sr && dr < sr + sn)
      || (sr > dr && sr < dr + dn))
    return;




  if (vd->e[sr].mode == VOIDmode)
    set_value_regno (sr, vd->e[dr].mode, vd);
  else if (sn < (unsigned int) (((sr) >= 8 && (sr) <= (8 + 7)) || (((sr) >= (20 + 1) && (sr) <= ((20 + 1) + 7)) || ((sr) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (sr) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((sr) >= (((20 + 1) + 7) + 1) && (sr) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (vd->e[sr].mode)]) == MODE_COMPLEX_INT || (mode_class[(int) (vd->e[sr].mode)]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((vd->e[sr].mode) == TFmode ? ((target_flags & 0x00100000) ? 2 : 3) : (vd->e[sr].mode) == TCmode ? ((target_flags & 0x00100000) ? 4 : 6) : (((mode_size[(int) (vd->e[sr].mode)]) + ((target_flags & 0x00100000) ? 8 : 4) - 1) / ((target_flags & 0x00100000) ? 8 : 4))))
    && ((mode_size[(int) (vd->e[sr].mode)]) > ((target_flags & 0x00100000) ? 8 : 4)
        ? 0 : 0))
    return;




  else if (sn > (unsigned int) (((sr) >= 8 && (sr) <= (8 + 7)) || (((sr) >= (20 + 1) && (sr) <= ((20 + 1) + 7)) || ((sr) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (sr) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((sr) >= (((20 + 1) + 7) + 1) && (sr) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (vd->e[sr].mode)]) == MODE_COMPLEX_INT || (mode_class[(int) (vd->e[sr].mode)]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((vd->e[sr].mode) == TFmode ? ((target_flags & 0x00100000) ? 2 : 3) : (vd->e[sr].mode) == TCmode ? ((target_flags & 0x00100000) ? 4 : 6) : (((mode_size[(int) (vd->e[sr].mode)]) + ((target_flags & 0x00100000) ? 8 : 4) - 1) / ((target_flags & 0x00100000) ? 8 : 4)))))
    return;



  vd->e[dr].oldest_regno = vd->e[sr].oldest_regno;

  for (i = sr; vd->e[i].next_regno != (~(unsigned int) 0); i = vd->e[i].next_regno)
    continue;
  vd->e[i].next_regno = dr;


  validate_value_data (vd);

}
