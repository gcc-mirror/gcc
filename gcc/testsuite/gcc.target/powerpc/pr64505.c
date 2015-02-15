/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } { "*" } { "" } } */
/* { dg-options "-O2 -mpowerpc64" } */

/*
 * (below is inlined and simplified from previously included headers)
 */

struct fltcom_st {
    short fltbuf[950];
} fltcom_  __attribute__((common))  ;
#define CM_PLIBOR (*(((double *)&fltcom_ + 1)))
#define CM_QMRG (*(((double *)&fltcom_ + 2)))

struct fltcom2_st {
    short fltbuf2[56];
} fltcom2_  __attribute__((common))  ;
#define CM_FLPRV ((short *)&fltcom2_ + 17)
#define CM_FLNXT ((short *)&fltcom2_ + 20)
#define CM_FLCPN (*(((double *)&fltcom2_)))
#define CM_FLCNT (*(((short *)&fltcom2_ + 12)))

struct aidatcm_st {
    double cm_aid, cm_ext, cm_basis;
    short cm_aiday, cm_exday, cm_dperd, cm_aiexf, cm_aidex, cm_aiok,
            cm_aigdo, cm_aildo, cm_prev[3], cm_next[3], cm_aid_pad[2];
    double cm_rvgfact, cm_ai1st, cm_ai2nd;
    int cm_aieurok;
} aidatcm_  __attribute__((common))  ;
#define CM_EXDAY aidatcm_.cm_exday
#define CM_BASIS aidatcm_.cm_basis
#define CM_PREV aidatcm_.cm_prev

struct cshfcm_st {
    short bufff[10862];
} cshfcm_  __attribute__((common))  ;
#define CM_FNUM (*(((short *)&cshfcm_ + 9038)))
#define CM_FIFLX ((double *)&cshfcm_ + 1)
#define CM_FEXTX ((double *)&cshfcm_ + 1201)
#define CM_FSHDT ((short *)&cshfcm_ + 7230)

struct calctsdb_st {
    short calctsdbbuff[115];
} calctsdb_  __attribute__((common))  ;
#define CM_CTUP_GOOD_TO_GO (*(((short *)&calctsdb_ + 16)))
#define CM_PAYMENT_FREQUENCY (*(((short *)&calctsdb_ + 61)))
#define CM_DISCOUNTING_DAYTYP (*(((short *)&calctsdb_ + 59)))

struct cf600cm_st {
    short bufcf[14404];
} cf600cm_  __attribute__((common)) ;
#define CM_FLT_RFIXRATES ((double *)&cf600cm_ + 600)

typedef struct { int id; int type; const char *name; } bregdb_bitinfo_t;

int
bregdb_eval_bbitcxt_bool_rv(const bregdb_bitinfo_t * const bbit,
                            const int bbit_default,
                            const void * const bregucxt);

static const bregdb_bitinfo_t bbit_calc_dr_d33 =
  { 160667, 5, "bbit_calc_dr_d33" };
#define bbit_calc_dr_d33__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d33, 0, 0)
static const bregdb_bitinfo_t bbit_calc_sx_b24 =
  { 158854, 5, "bbit_calc_sx_b24" };
#define bbit_calc_sx_b24__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_sx_b24, 0, 0)
static const bregdb_bitinfo_t bbit_calc_dr_d36 =
  { 161244, 5, "bbit_calc_dr_d36" };
#define bbit_calc_dr_d36__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d36, 0, 0)
static const bregdb_bitinfo_t bbit_calc_dr_d37 =
  { 161315, 5, "bbit_calc_dr_d37" };
#define bbit_calc_dr_d37__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d37, 0, 0)
static const bregdb_bitinfo_t bbit_calc_dr_d47 =
  { 163259, 5, "bbit_calc_dr_d47" };
#define bbit_calc_dr_d47__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d47, 0, 0)
static const bregdb_bitinfo_t bbit_calc_dr_d46 =
  { 163239, 5, "bbit_calc_dr_d46" };
#define bbit_calc_dr_d46__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d46, 0, 0)
static const bregdb_bitinfo_t bbit_calc_dr_d62 =
  { 166603, 5, "bbit_calc_dr_d62" };
#define bbit_calc_dr_d62__value() \
  bregdb_eval_bbitcxt_bool_rv(&bbit_calc_dr_d62, 0, 0)



int dtyp_is_actact_(short *daytyp);
double rnd_trunc_numb(double in, short num_digits, short rnd_or_trunc);
void datetrn_(const short* dt, short* dt2);
short difday_(short* daytyp_in, short* srtdti, short* enddti, short* ercode);


double pow(double x, double y);


/*
 * (above is inlined and simplified from previously included headers)
 */


void calc_1566(
  short  sCalcType,
  short  sDayType,
  short  sFreq,
  short  asSettleDt[3],
  short  asMtyDt[3],
  short  asIssueDt[3],
  short  asFCpnDt[3],
  double dCpn,
  short  *psNoPer,
  double *pdExt,
  double *pdAI,
  double *pdAI2,
  double *pdFCpn,
  short  *psRcode)
{

    short ercode = 0;
    int isactact;
    short days_to_next_cpn = 0;
    const short discDaytype = CM_DISCOUNTING_DAYTYP;

    if(bbit_calc_sx_b24__value())
        isactact = (dtyp_is_actact_(&sDayType) != 0);
    else
        isactact = (sDayType == 1 || sDayType == 10);

    short days_in_current_period = difday_(&sDayType,CM_FLPRV,CM_FLNXT,&ercode);
    const short sfreq1 = (CM_CTUP_GOOD_TO_GO == 1 && CM_PAYMENT_FREQUENCY == 1);

    for (int j = 0; j < CM_FNUM; j++) {

        if(j == 0) {
            days_to_next_cpn = difday_(&sDayType,asSettleDt,CM_FLNXT,&ercode);

            if(isactact) {
                CM_FIFLX[j] = CM_FLCPN / sFreq;
                CM_FEXTX[j] = (double)days_to_next_cpn / (double)days_in_current_period;
            }
            else {
                CM_FIFLX[j] = CM_FLCPN * days_in_current_period;
                CM_FEXTX[j] = (double)days_to_next_cpn / (double)(1/sfreq1);
            }

            if(CM_FNUM == 1) {
                CM_FEXTX[j] = (double)days_to_next_cpn / ((double)1/sfreq1);
            }
        }
        else {

            short days_from_settle, days_in_period;

            if(bbit_calc_dr_d46__value()){
             days_from_settle = difday_(&sDayType,asSettleDt,
                                             &CM_FSHDT[j*3],&ercode);
             days_in_period =  difday_(&sDayType,&CM_FSHDT[(j-1)*3],
                                            &CM_FSHDT[j*3],&ercode);
            }

            double cpn_rate = CM_PLIBOR;

            if(bbit_calc_dr_d62__value()) {
              if(j < CM_FLCNT && CM_FLT_RFIXRATES[j] != 0) cpn_rate = CM_FLT_RFIXRATES[j];
            }
            else {
              if(j < CM_FLCNT ) cpn_rate = CM_FLT_RFIXRATES[j];
            }

            if(bbit_calc_dr_d37__value()&& j >= CM_FLCNT && sCalcType == 1570) {
                cpn_rate = CM_PLIBOR + CM_QMRG;

                if(bbit_calc_dr_d36__value()){
                double projected_rate = pow((1 + CM_PLIBOR/100.0),
                                            (days_in_period)) - 1;

                projected_rate = projected_rate + CM_QMRG/100.0 * days_in_period;
                cpn_rate = 100 * projected_rate * (1/days_in_period);
                }
            }


            if(isactact) {
                CM_FIFLX[j] = cpn_rate / sFreq;
                CM_FEXTX[j] = CM_FEXTX[j-1] + 1;

                if(bbit_calc_dr_d46__value() && discDaytype != 0) {
                    CM_FEXTX[j] = (double)days_from_settle / (double)(1/sfreq1);
                }
            }
            else {
                if(!bbit_calc_dr_d46__value()){
                days_from_settle = difday_(&sDayType,asSettleDt,
                                               &CM_FSHDT[j*3],&ercode);
                days_in_period =  difday_(&sDayType,&CM_FSHDT[(j-1)*3],
                                               &CM_FSHDT[j*3],&ercode);

                }

                CM_FIFLX[j] = cpn_rate * days_in_period;
                CM_FEXTX[j] = (double)days_from_settle / (double)(1/sfreq1);
            }

        }

        if(bbit_calc_dr_d33__value() && CM_CTUP_GOOD_TO_GO != 0) {
            CM_FIFLX[j] = rnd_trunc_numb (CM_FIFLX[j], 0, 0);
        }

    }


    short accrued_days = difday_(&sDayType,CM_FLPRV,asSettleDt,&ercode);

    if(!bbit_calc_dr_d47__value()) {
    if(isactact) {
        *pdAI = (CM_FLCPN / sFreq)* accrued_days / ((double)days_in_current_period);
    }
    else{
        *pdAI = (CM_FLCPN / sFreq)* accrued_days / ((double)1/sFreq);
    }
    }

    CM_EXDAY = days_to_next_cpn;
    CM_BASIS = days_in_current_period;
    datetrn_(CM_FLPRV,CM_PREV);
}
