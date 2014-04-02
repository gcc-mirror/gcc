/* { dg-do compile } */
/* { dg-options "-w -O3 -mcpu=cortex-a53" } */
typedef struct __sFILE __FILE;
typedef __FILE FILE;
typedef int atom_id;
typedef float real;
typedef real rvec[3];
typedef real matrix[3][3];
enum {
  ebCGS,ebMOLS,ebSBLOCKS,ebNR
};
enum {
  efepNO, efepYES, efepNR
};
enum {
  esolNO, esolMNO, esolWATER, esolWATERWATER, esolNR
};
typedef struct {
  int nr;
  atom_id *index;
  atom_id *a;
} t_block;
enum {
  F_LJ,
  F_LJLR,
  F_SR,
  F_LR,
  F_DVDL,
};
typedef struct {
  t_block excl;
} t_atoms;
typedef struct {
  t_atoms atoms;
  t_block blocks[ebNR];
} t_topology;
typedef struct {
} t_nsborder;
extern FILE *debug;
typedef struct {
} t_nrnb;
typedef struct {
  int nri,maxnri;
  int nrj,maxnrj;
  int maxlen;
  int solvent;
  int *gid;
  int *jindex;
  atom_id *jjnr;
  int *nsatoms;
} t_nblist;
typedef struct {
  int nrx,nry,nrz;
} t_grid;
typedef struct {
} t_commrec;
enum { eNL_VDWQQ, eNL_VDW, eNL_QQ,
       eNL_VDWQQ_FREE, eNL_VDW_FREE, eNL_QQ_FREE,
       eNL_VDWQQ_SOLMNO, eNL_VDW_SOLMNO, eNL_QQ_SOLMNO,
       eNL_VDWQQ_WATER, eNL_QQ_WATER,
       eNL_VDWQQ_WATERWATER, eNL_QQ_WATERWATER,
       eNL_NR };
typedef struct {
  real rlist,rlistlong;
  real rcoulomb_switch,rcoulomb;
  real rvdw_switch,rvdw;
  int efep;
  int cg0,hcg;
  int *solvent_type;
  int *mno_index;
  rvec *cg_cm;
  t_nblist nlist_sr[eNL_NR];
  t_nblist nlist_lr[eNL_NR];
  int bTwinRange;
  rvec *f_twin;
  int *eg_excl;
} t_forcerec;
typedef struct {
  real *chargeA,*chargeB,*chargeT;
  int *bPerturbed;
  int *typeA,*typeB;
  unsigned short *cTC,*cENER,*cACC,*cFREEZE,*cXTC,*cVCM;
} t_mdatoms;
enum { egCOUL, egLJ, egBHAM, egLR, egLJLR, egCOUL14, egLJ14, egNR };
typedef struct {
  real *ee[egNR];
} t_grp_ener;
typedef struct {
  t_grp_ener estat;
} t_groups;
typedef unsigned long t_excl;
static void reset_nblist(t_nblist *nl)
{
  nl->nri = 0;
  nl->nrj = 0;
  nl->maxlen = 0;
  if (nl->maxnri > 0) {
    nl->gid[0] = -1;
    if (nl->maxnrj > 1) {
      nl->jindex[0] = 0;
      nl->jindex[1] = 0;
    }
  }
}
static void reset_neighbor_list(t_forcerec *fr,int bLR,int eNL)
{
    reset_nblist(&(fr->nlist_lr[eNL]));
}
static void close_i_nblist(t_nblist *nlist)
{
  int nri = nlist->nri;
  int len;
  nlist->jindex[nri+1] = nlist->nrj;
  len=nlist->nrj - nlist->jindex[nri];
  if (nlist->solvent==esolMNO)
    len *= nlist->nsatoms[3*nri];
  if(len > nlist->maxlen)
    nlist->maxlen = len;
}
static void close_nblist(t_nblist *nlist)
{
  if (nlist->maxnri > 0) {
    int nri = nlist->nri;
    if ((nlist->jindex[nri+1] > nlist->jindex[nri]) &&
 (nlist->gid[nri] != -1)) {
      nlist->nri++;
      nlist->jindex[nri+2] = nlist->nrj;
    }
  }
}
static void close_neighbor_list(t_forcerec *fr,int bLR,int eNL)
{
    close_nblist(&(fr->nlist_lr[eNL]));
}
static void add_j_to_nblist(t_nblist *nlist,atom_id j_atom)
{
  int nrj=nlist->nrj;
  nlist->jjnr[nrj] = j_atom;
  nlist->nrj ++;
}
static void put_in_list(int bHaveLJ[],
          int ngid,t_mdatoms *md,
          int icg,int jgid,int nj,atom_id jjcg[],
          atom_id index[],
          t_excl bExcl[],int shift,
          t_forcerec *fr,int bLR,
          int bVDWOnly,int bCoulOnly)
{
  t_nblist *vdwc,*vdw,*coul;
  t_nblist *vdwc_ww=((void *)0),*coul_ww=((void *)0);
  t_nblist *vdwc_free=((void *)0),*vdw_free=((void *)0),*coul_free=((void *)0);
  int i,j,jcg,igid,gid,ind_ij;
  atom_id jj,jj0,jj1,i_atom;
  int i0,nicg,len;
  int *type,*typeB;
  unsigned short *cENER;
  real *charge,*chargeB;
  real qi,qiB,qq,rlj;
  int bWater,bMNO,bFree,bFreeJ,bNotEx,*bPert;
  charge = md->chargeA;
  chargeB = md->chargeB;
  type = md->typeA;
  typeB = md->typeB;
  cENER = md->cENER;
  bPert = md->bPerturbed;
  i0 = index[icg];
  nicg = index[icg+1]-i0;
  bMNO = (fr->solvent_type[icg] == esolMNO);
  if (bLR) {
    if (bWater) {
      vdw = &fr->nlist_lr[eNL_VDW];
      coul = &fr->nlist_lr[eNL_QQ_WATER];
      vdwc_ww = &fr->nlist_lr[eNL_VDWQQ_WATERWATER];
    } else if(bMNO) {
      vdwc = &fr->nlist_lr[eNL_VDWQQ_SOLMNO];
    }
    if (fr->efep != efepNO) {
      vdw_free = &fr->nlist_lr[eNL_VDW_FREE];
      coul_free = &fr->nlist_lr[eNL_QQ_FREE];
    }
  }
  else {
    if (bWater) {
    } else if(bMNO) {
      vdwc = &fr->nlist_sr[eNL_VDWQQ_SOLMNO];
    }
    if (fr->efep != efepNO) {
      vdwc_free = &fr->nlist_sr[eNL_VDWQQ_FREE];
    }
  }
  if (fr->efep==efepNO) {
    if (bWater) {
      igid = cENER[i_atom];
      gid = ((igid < jgid) ? (igid*ngid+jgid) : (jgid*ngid+igid));
      if (!bCoulOnly && !bVDWOnly) {
 new_i_nblist(vdwc,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,((void *)0));
 new_i_nblist(vdwc_ww,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,((void *)0));
      }
      if (!bCoulOnly)
 new_i_nblist(vdw,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,((void *)0));
      if (!bVDWOnly) {
 new_i_nblist(coul,bLR ? F_LR : F_SR,i_atom,shift,gid,((void *)0));
 new_i_nblist(coul_ww,bLR ? F_LR : F_SR,i_atom,shift,gid,((void *)0));
      }
      for(j=0; (j<nj); j++) {
 jcg=jjcg[j];
 if (jcg==icg)
 jj0 = index[jcg];
 if (bWater && (fr->solvent_type[jcg] == esolWATER)) {
   if (bVDWOnly)
     add_j_to_nblist(vdw,jj0);
   else {
       add_j_to_nblist(coul_ww,jj0);
       add_j_to_nblist(vdwc_ww,jj0);
   }
 } else {
   jj1 = index[jcg+1];
   if (bCoulOnly) {
     for(jj=jj0; (jj<jj1); jj++) {
       if (fabs(charge[jj]) > 1.2e-38)
  add_j_to_nblist(coul,jj);
     }
   } else if (bVDWOnly) {
     for(jj=jj0; (jj<jj1); jj++)
       if (bHaveLJ[type[jj]])
  add_j_to_nblist(vdw,jj);
   } else {
     for(jj=jj0; (jj<jj1); jj++) {
       if (bHaveLJ[type[jj]]) {
  if (fabs(charge[jj]) > 1.2e-38)
    add_j_to_nblist(vdwc,jj);
    add_j_to_nblist(vdw,jj);
       } else if (fabs(charge[jj]) > 1.2e-38)
  add_j_to_nblist(coul,jj);
     }
   }
 }
      }
      close_i_nblist(vdw);
      close_i_nblist(coul);
      close_i_nblist(vdwc);
      close_i_nblist(coul_ww);
      close_i_nblist(vdwc_ww);
    } else if (bMNO) {
      igid = cENER[i_atom];
      gid = ((igid < jgid) ? (igid*ngid+jgid) : (jgid*ngid+igid));
      if (!bCoulOnly && !bVDWOnly)
 new_i_nblist(vdwc,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,
       &(fr->mno_index[icg*3]));
      if (!bCoulOnly)
 new_i_nblist(vdw,bLR ? F_LR : F_SR,i_atom,shift,gid,
       &(fr->mno_index[icg*3]));
      if (!bVDWOnly)
 new_i_nblist(coul,bLR ? F_LR : F_SR,i_atom,shift,gid,
       &(fr->mno_index[icg*3]));
      for(j=0; (j<nj); j++) {
 jcg=jjcg[j];
 if (jcg == icg)
 jj0 = index[jcg];
 jj1=index[jcg+1];
 for(jj=jj0; (jj<jj1); jj++) {
   if (bCoulOnly) {
     if (fabs(charge[jj]) > 1.2e-38)
       add_j_to_nblist(coul,jj);
   } else if (bVDWOnly) {
     if (bHaveLJ[type[jj]])
       add_j_to_nblist(vdw,jj);
   } else {
     if (bHaveLJ[type[jj]]) {
       if (fabs(charge[jj]) > 1.2e-38)
  add_j_to_nblist(vdwc,jj);
  add_j_to_nblist(vdw,jj);
     } else if (fabs(charge[jj]) > 1.2e-38)
       add_j_to_nblist(coul,jj);
   }
 }
 close_i_nblist(vdw);
 close_i_nblist(coul);
 close_i_nblist(vdwc);
      }
    } else {
      for(i=0; i<nicg; i++) {
 igid = cENER[i_atom];
 gid = ((igid < jgid) ? (igid*ngid+jgid) : (jgid*ngid+igid));
 qi = charge[i_atom];
 if (!bCoulOnly && !bVDWOnly)
   new_i_nblist(vdwc,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,((void *)0));
 if (!bCoulOnly)
   new_i_nblist(vdw,bLR ? F_LR : F_SR,i_atom,shift,gid,((void *)0));
 if (!bVDWOnly)
   new_i_nblist(coul,bLR ? F_LR : F_SR,i_atom,shift,gid,((void *)0));
 if (!(bVDWOnly || fabs(qi)<1.2e-38) || !(bCoulOnly || !bHaveLJ[type[i_atom]])) {
   for(j=0; (j<nj); j++) {
     jcg=jjcg[j];
     if (jcg == icg)
       jj0 = i0 + i + 1;
     else
       jj0 = index[jcg];
     jj1=index[jcg+1];
     for(jj=jj0; jj<jj1; jj++) {
       bNotEx = !((int) ((bExcl)[((atom_id) (jj))] & (1<<((atom_id) (i)))));
       if (bNotEx) {
  if (bCoulOnly) {
                  if (fabs(charge[jj]) > 1.2e-38)
                    add_j_to_nblist(coul,jj);
  } else if (bVDWOnly) {
    if (bHaveLJ[type[jj]])
      add_j_to_nblist(vdw,jj);
  } else {
    if (bHaveLJ[type[jj]]) {
      if (fabs(qi) > 1.2e-38 && (fabs(charge[jj]) > 1.2e-38))
        add_j_to_nblist(vdwc,jj);
        add_j_to_nblist(vdw,jj);
    } else if (fabs(qi) > 1.2e-38 && (fabs(charge[jj]) > 1.2e-38))
      add_j_to_nblist(coul,jj);
  }
       }
     }
   }
 }
 close_i_nblist(vdw);
 close_i_nblist(coul);
 close_i_nblist(vdwc);
      }
    }
  } else {
    for(i=0; i<nicg; i++) {
      igid = cENER[i_atom];
      gid = ((igid < jgid) ? (igid*ngid+jgid) : (jgid*ngid+igid));
      qi = charge[i_atom];
      qiB = chargeB[i_atom];
      if (!bCoulOnly && !bVDWOnly)
 new_i_nblist(vdwc,bLR ? F_LJLR : F_LJ,i_atom,shift,gid,
       bMNO ? &(fr->mno_index[icg*3]) : ((void *)0));
      if (!bCoulOnly)
 new_i_nblist(vdw,bLR ? F_LR : F_SR,i_atom,shift,gid,
       bMNO ? &(fr->mno_index[icg*3]) : ((void *)0));
 new_i_nblist(coul,bLR ? F_LR : F_SR,i_atom,shift,gid,
       bMNO ? &(fr->mno_index[icg*3]) : ((void *)0));
      new_i_nblist(vdw_free,F_DVDL,i_atom,shift,gid,((void *)0));
      new_i_nblist(coul_free,F_DVDL,i_atom,shift,gid,((void *)0));
      new_i_nblist(vdwc_free,F_DVDL,i_atom,shift,gid,((void *)0));
      if (!(bVDWOnly || (fabs(qi)<1.2e-38 && fabs(qiB)<1.2e-38)) ||
   !(bCoulOnly || (!bHaveLJ[type[i_atom]] && !bHaveLJ[typeB[i_atom]]))) {
 for(j=0; (j<nj); j++) {
   jcg=jjcg[j];
   if (jcg == icg)
     jj0 = i0 + i + 1;
   else
     jj0 = index[jcg];
   jj1=index[jcg+1];
   bFree = bPert[i_atom];
   for(jj=jj0; (jj<jj1); jj++) {
     bFreeJ = bFree || bPert[jj];
     if ((!bWater && !bMNO) || i==0 || bFreeJ) {
       bNotEx = !((int) ((bExcl)[((atom_id) (jj))] & (1<<((atom_id) (i)))));
       if (bNotEx) {
                if (bFreeJ) {
    if (bCoulOnly)
      add_j_to_nblist(coul_free,jj);
    else if (bVDWOnly)
      add_j_to_nblist(vdw_free,jj);
      add_j_to_nblist(vdwc_free,jj);
  } else if (bCoulOnly) {
                    add_j_to_nblist(coul,jj);
                } else if (bVDWOnly) {
                  if (bHaveLJ[type[jj]])
                    add_j_to_nblist(vdw,jj);
                } else {
                  if (bHaveLJ[type[jj]]) {
                    if (fabs(qi) > 1.2e-38 && (fabs(charge[jj]) > 1.2e-38))
                      add_j_to_nblist(vdwc,jj);
                      add_j_to_nblist(vdw,jj);
                  } else if (fabs(qi) > 1.2e-38 && (fabs(charge[jj]) > 1.2e-38))
                    add_j_to_nblist(coul,jj);
                }
       }
     }
   }
 }
      }
      close_i_nblist(vdw);
      close_i_nblist(coul);
      close_i_nblist(vdwc);
      if (bWater && (i==0)) {
 close_i_nblist(coul_ww);
 close_i_nblist(vdwc_ww);
      }
      close_i_nblist(vdw_free);
      close_i_nblist(coul_free);
      close_i_nblist(vdwc_free);
    }
  }
}
static void setexcl(atom_id start,atom_id end,t_block *excl,int b,
      t_excl bexcl[])
{
  atom_id i,k;
  if (b) {
    for(i=start; i<end; i++) {
      for(k=excl->index[i]; k<excl->index[i+1]; k++) {
 (bexcl)[((atom_id) (excl->a[k]))] |= (1<<((atom_id) (i-start)));
      }
    }
  }
}
int calc_naaj(int icg,int cgtot)
{
  int naaj;
  if ((cgtot % 2) == 1) {
    naaj = 1+(cgtot/2);
  }
  else if ((cgtot % 4) == 0) {
    if (icg < cgtot/2) {
      if ((icg % 2) == 0)
 naaj=1+(cgtot/2);
    }
    else {
      if ((icg % 2) == 1)
 naaj=1+(cgtot/2);
    }
  }
  else {
    if ((icg % 2) == 0)
      naaj=1+(cgtot/2);
    else
      naaj=cgtot/2;
  }
  return naaj;
}
static void get_dx(int Nx,real gridx,real grid_x,real rc2,real x,
         int *dx0,int *dx1,real *dcx2)
{
  real dcx,tmp;
  int xgi,xgi0,xgi1,i;
  xgi = (int)(Nx+x*grid_x)-Nx;
  if (xgi < 0) {
    *dx0 = 0;
    *dx1 = -1;
  } else if (xgi >= Nx) {
    *dx0 = Nx;
    *dx1 = Nx-1;
  } else {
    dcx2[xgi] = 0;
    *dx0 = xgi;
    xgi0 = xgi-1;
    *dx1 = xgi;
    xgi1 = xgi+1;
  }
  for(i=xgi0; i>=0; i--) {
     dcx = (i+1)*gridx-x;
     tmp = dcx*dcx;
     if (tmp >= rc2)
     *dx0 = i;
     dcx2[i] = tmp;
  }
  for(i=xgi1; i<Nx; i++) {
     dcx = i*gridx-x;
     tmp = dcx*dcx;
     if (tmp >= rc2)
     *dx1 = i;
     dcx2[i] = tmp;
  }
}
static void do_longrange(FILE *log,t_commrec *cr,t_topology *top,t_forcerec *fr,
    int ngid,t_mdatoms *md,int icg,
    int jgid,int nlr,
    atom_id lr[],t_excl bexcl[],int shift,
    rvec x[],rvec box_size,t_nrnb *nrnb,
    real lambda,real *dvdlambda,
    t_groups *grps,int bVDWOnly,int bCoulOnly,
    int bDoForces,int bHaveLJ[])
{
  int i;
  for(i=0; (i<eNL_NR); i++) {
    if ((fr->nlist_lr[i].nri > fr->nlist_lr[i].maxnri-32) || bDoForces) {
      close_neighbor_list(fr,1,i);
      do_fnbf(log,cr,fr,x,fr->f_twin,md,
       grps->estat.ee[egLJLR],grps->estat.ee[egLR],box_size,
       nrnb,lambda,dvdlambda,1,i);
      reset_neighbor_list(fr,1,i);
    }
  }
  if (!bDoForces) {
    put_in_list(bHaveLJ,ngid,md,icg,jgid,nlr,lr,top->blocks[ebCGS].index,
                              bexcl,shift,fr,
  1,bVDWOnly,bCoulOnly);
  }
}
static int ns5_core(FILE *log,t_commrec *cr,t_forcerec *fr,int cg_index[],
      matrix box,rvec box_size,int ngid,
      t_topology *top,t_groups *grps,
      t_grid *grid,rvec x[],t_excl bexcl[],int *bExcludeAlleg,
      t_nrnb *nrnb,t_mdatoms *md,
      real lambda,real *dvdlambda,
      int bHaveLJ[])
{
  static atom_id **nl_lr_ljc,**nl_lr_one,**nl_sr=((void *)0);
  static int *nlr_ljc,*nlr_one,*nsr;
  static real *dcx2=((void *)0),*dcy2=((void *)0),*dcz2=((void *)0);
  t_block *cgs=&(top->blocks[ebCGS]);
  unsigned short *gid=md->cENER;
  int tx,ty,tz,dx,dy,dz,cj;
  int dx0,dx1,dy0,dy1,dz0,dz1;
  int Nx,Ny,Nz,shift=-1,j,nrj,nns,nn=-1;
  real gridx,gridy,gridz,grid_x,grid_y,grid_z;
  int icg=-1,iicg,cgsnr,i0,nri,naaj,min_icg,icg_naaj,jjcg,cgj0,jgid;
  int bVDWOnly,bCoulOnly;
  rvec xi,*cgcm;
  real r2,rs2,rvdw2,rcoul2,rm2,rl2,XI,YI,ZI,dcx,dcy,dcz,tmp1,tmp2;
  int *i_eg_excl;
  int use_twinrange,use_two_cutoffs;
  cgsnr = cgs->nr;
  rs2 = ((fr->rlist)*(fr->rlist));
  if (fr->bTwinRange) {
    rvdw2 = ((fr->rvdw)*(fr->rvdw));
    rcoul2 = ((fr->rcoulomb)*(fr->rcoulomb));
  } else {
  }
  rm2 = (((rvdw2) < (rcoul2)) ? (rvdw2) : (rcoul2) );
  rl2 = (((rvdw2) > (rcoul2)) ? (rvdw2) : (rcoul2) );
  use_twinrange = (rs2 < rm2);
  use_two_cutoffs = (rm2 < rl2);
  bVDWOnly = (rvdw2 > rcoul2);
  bCoulOnly = !bVDWOnly;
  if (nl_sr == ((void *)0)) {
    (nl_sr)=save_calloc("nl_sr","ns.c",1341, (ngid),sizeof(*(nl_sr)));
    (nsr)=save_calloc("nsr","ns.c",1343, (ngid),sizeof(*(nsr)));
    (nlr_ljc)=save_calloc("nlr_ljc","ns.c",1344, (ngid),sizeof(*(nlr_ljc)));
    (nlr_one)=save_calloc("nlr_one","ns.c",1345, (ngid),sizeof(*(nlr_one)));
    if (use_twinrange)
      (nl_lr_ljc)=save_calloc("nl_lr_ljc","ns.c",1349, (ngid),sizeof(*(nl_lr_ljc)));
    if (use_two_cutoffs)
      (nl_lr_one)=save_calloc("nl_lr_one","ns.c",1353, (ngid),sizeof(*(nl_lr_one)));
    for(j=0; (j<ngid); j++) {
      (nl_sr[j])=save_calloc("nl_sr[j]","ns.c",1356, (1024),sizeof(*(nl_sr[j])));
      if (use_twinrange)
 (nl_lr_ljc[j])=save_calloc("nl_lr_ljc[j]","ns.c",1358, (1024),sizeof(*(nl_lr_ljc[j])));
      if (use_two_cutoffs)
 (nl_lr_one[j])=save_calloc("nl_lr_one[j]","ns.c",1360, (1024),sizeof(*(nl_lr_one[j])));
    }
    if (debug)
      fprintf(debug,"ns5_core: rs2 = %g, rvdw2 = %g, rcoul2 = %g (nm^2)\n",
       rs2,rvdw2,rcoul2);
  }
  cgcm = fr->cg_cm;
  Nx = grid->nrx;
  Ny = grid->nry;
  if (dcx2 == ((void *)0)) {
    (dcx2)=save_calloc("dcx2","ns.c",1379, (Nx*2),sizeof(*(dcx2)));
    (dcy2)=save_calloc("dcy2","ns.c",1380, (Ny*2),sizeof(*(dcy2)));
    (dcz2)=save_calloc("dcz2","ns.c",1381, (Nz*2),sizeof(*(dcz2)));
  }
  gridx = box[0][0]/grid->nrx;
  gridy = box[1][1]/grid->nry;
  gridz = box[2][2]/grid->nrz;
  grid_x = 1/gridx;
  grid_y = 1/gridy;
  grid_z = 1/gridz;
  for(iicg=fr->cg0; (iicg < fr->hcg); iicg++) {
    icg = cg_index[iicg];
    if (icg != iicg)
      fatal_error(0,"icg = %d, iicg = %d, file %s, line %d",icg,iicg,"ns.c",
    1408);
    if(bExcludeAlleg[icg])
    i_eg_excl = fr->eg_excl + ngid*gid[cgs->index[icg]];
    setexcl(cgs->index[icg],cgs->index[icg+1],&top->atoms.excl,1,bexcl);
    naaj = calc_naaj(icg,cgsnr);
    icg_naaj = icg+naaj;
    for (tz=-1; tz<=1; tz++) {
      ZI = cgcm[icg][2]+tz*box[2][2];
      get_dx(Nz,gridz,grid_z,rcoul2,ZI,&dz0,&dz1,dcz2);
      if (dz0 > dz1)
      for (ty=-1; ty<=1; ty++) {
 YI = cgcm[icg][1]+ty*box[1][1]+tz*box[2][1];
 get_dx(Ny,gridy,grid_y,rcoul2,YI,&dy0,&dy1,dcy2);
        for (tx=-1; tx<=1; tx++) {
   get_dx(Nx,gridx,grid_x,rcoul2,XI,&dx0,&dx1,dcx2);
   shift=((2*1 +1)*((2*1 +1)*((tz)+1)+(ty)+1)+(tx)+1);
   for (dx=dx0; (dx<=dx1); dx++) {
     for (dy=dy0; (dy<=dy1); dy++) {
  for (dz=dz0; (dz<=dz1); dz++) {
    if (tmp2 > dcz2[dz]) {
      for (j=0; (j<nrj); j++) {
        if (((jjcg >= icg) && (jjcg < icg_naaj)) ||
     ((jjcg < min_icg))) {
   if (r2 < rl2) {
     if (!i_eg_excl[jgid]) {
       if (r2 < rs2) {
         if (nsr[jgid] >= 1024) {
    put_in_list(bHaveLJ,ngid,md,icg,jgid,
         nsr[jgid],nl_sr[jgid],
         cgs->index, bexcl,
         shift,fr,0,0,0);
         }
       } else if (r2 < rm2) {
       } else if (use_two_cutoffs) {
         if (nlr_one[jgid] >= 1024) {
    do_longrange(log,cr,top,fr,ngid,md,icg,jgid,
          nlr_one[jgid],
          nl_lr_one[jgid],bexcl,shift,x,
          box_size,nrnb,
          lambda,dvdlambda,grps,
          bVDWOnly,bCoulOnly,0,
          bHaveLJ);
         }
       }
     }
   }
        }
      }
    }
  }
     }
   }
 }
      }
    }
  }
}
int search_neighbours(FILE *log,t_forcerec *fr,
        rvec x[],matrix box,
        t_topology *top,t_groups *grps,
        t_commrec *cr,t_nsborder *nsb,
        t_nrnb *nrnb,t_mdatoms *md,
        real lambda,real *dvdlambda)
{
  static t_grid *grid=((void *)0);
  static t_excl *bexcl;
  static int *bHaveLJ;
  static int *cg_index=((void *)0),*slab_index=((void *)0);
  static int *bExcludeAlleg;
  rvec box_size;
  int i,j,m,ngid;
  int nsearch;
    nsearch = ns5_core(log,cr,fr,cg_index,box,box_size,ngid,top,grps,
         grid,x,bexcl,bExcludeAlleg,nrnb,md,lambda,dvdlambda,bHaveLJ);
}
