/* PR target/7784 */
/* Originator: Peter van Hoof <p.van-hoof@qub.ac.uk> */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ultrasparc" } */

typedef struct
{
  float EnergyErg;
  float ots;
} EmLine;

extern const int ipH_LIKE ;
extern const int ipHYDROGEN ;
extern const int ipH1s;
extern const int ipH2s;
extern const int ipH2p;

extern EmLine ****EmisLines;

typedef struct
{
  long n;
  long s;
  long l;
} Elevels;

extern struct t_iso
{
  float ***Pop2Ion;
  long int numLevels[2][30L];
} iso;

extern struct t_LineSave
{
  long int nsum;
  long int ndsum;
  long int nComment;
  long int npxdd;
  long int ipass;
  char chHoldComments[10][200];
} LineSave;

extern struct t_hydro
{
  int lgHydEmiss;
  float **pestrk ;
} hydro;

extern struct t_dense
{
  double DensityLaw[10];
  float frad[500];
  float fhden[500];
  float den0;
  double eden;
} dense;

extern struct t_abund
{
  float xIonFracs[30L +3][30L +1];
} abund;

extern struct t_CaseBHS
{
  long int nDensity[2][8] , ntemp[2][8] , ncut[2][8] ;
  int lgHCaseBOK[2][8];
} CaseBHS ;

extern struct t_smbeta
{
  float SimHBeta,
    cn4861,
    cn1216,
    sv4861,
    sv1216;
} smbeta;

extern struct t_phycon
{
  float te;
} phycon;


extern struct t_sphere
{
  int lgSphere;
  float covgeo;
} sphere;

void linadd(double xInten, float wavelength, char *chLab, char chInfo);

extern struct t_radiusVar
{
  int lgDrNeg;
  double dVeff;
} radius;

void lines_hydro(void)
{
  long int i, nelem, ipHi, ipLo;
  double hbetab, em , EmisFac, pump;
  char chLabel[5];

  linadd(abund.xIonFracs[ipHYDROGEN][1]*iso.Pop2Ion[ipH_LIKE][ipHYDROGEN][3]*hydro.pestrk[3][2]*3.025e-12, 6563,"Strk",'i');

  linadd(abund.xIonFracs[ipHYDROGEN][1]*iso.Pop2Ion[ipH_LIKE][ipHYDROGEN][4]*hydro.pestrk[4][2]*4.084e-12, 4861,"Strk",'i');

  linadd(abund.xIonFracs[ipHYDROGEN][1]*iso.Pop2Ion[ipH_LIKE][ipHYDROGEN][4]*hydro.pestrk[4][3]*1.059e-12, 18751,"Strk",'i');

  linadd(abund.xIonFracs[ipHYDROGEN][1]*iso.Pop2Ion[ipH_LIKE][ipHYDROGEN][5]*hydro.pestrk[5][4]*4.900e-13, 40512,"Strk",'i');

  ((void)((LineSave.ipass <1 || EmisLines[ipH_LIKE][ipHYDROGEN][ipH2p][ipH1s].ots>= 0.) || (__assert("LineSave.ipass <1 || EmisLines[ipH_LIKE][ipHYDROGEN][ipH2p][ipH1s].ots>= 0.", "lines_hydro.c", 118), 0)));

  linadd(EmisLines[ipH_LIKE][ipHYDROGEN][3][ipH2s].ots*EmisLines[ipH_LIKE][ipHYDROGEN][3][ipH2s].EnergyErg, 6563,"Dest",'i');

  linadd(EmisLines[ipH_LIKE][ipHYDROGEN][5][4].ots*EmisLines[ipH_LIKE][ipHYDROGEN][5][4].EnergyErg,40516, "Dest",'i');

  smbeta.SimHBeta = smbeta.SimHBeta/(float)radius.dVeff*sphere.covgeo;

  linadd(smbeta.SimHBeta,4861,"Q(H)",'i');

  smbeta.SimHBeta = smbeta.SimHBeta*(float)radius.dVeff/sphere.covgeo;

  for( nelem=0; nelem < 30L; nelem++ )
  {
    int iCase;
    for( iCase=0; iCase<2; ++iCase )
    {
      char chAB[2]={'A','B'};
      char chLab[5]="Ca  ";

      for( ipLo=1+iCase; ipLo<(((6)<(iso.numLevels[ipH_LIKE][nelem])) ? (6) : (5)); ++ipLo )
      {
        for( ipHi=ipLo+1; ipHi< (((ipLo+5)<(iso.numLevels[ipH_LIKE][nelem])) ? (ipLo+5) : (iso.numLevels[ipH_LIKE][nelem])); ++ipHi )
        {
          float wl;

          hbetab = HSRate( ipHi,ipLo , nelem+1, phycon.te , dense.eden, chAB[iCase] );
          if( hbetab<=0. )
            CaseBHS.lgHCaseBOK[iCase][nelem] = 0;

          if( !hydro.lgHydEmiss )
            hbetab *= abund.xIonFracs[nelem][nelem+1]*dense.eden;

         linadd(hbetab,wl,chLab,'i' );
        }
      }
    }
  }
}
