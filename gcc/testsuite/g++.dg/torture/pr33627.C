/* { dg-do compile } */
/* { dg-additional-options "-Wno-return-type" } */

typedef unsigned int UT_uint32;
typedef UT_uint32 PT_DocPosition;
typedef UT_uint32 PT_BlockOffset;
typedef enum _PTStruxType { PTX_Block } PTStruxType;
typedef UT_uint32 PL_ListenerId;
typedef const void * PL_StruxFmtHandle;
class PX_ChangeRecord;
class pf_Frag {
  public:
   typedef enum _PFType { PFT_Object } PFType;
   inline PFType getType(void) const { return PFType(); }
   inline pf_Frag * getNext(void) const { return 0; }
   PT_DocPosition getPos(void) const { return PT_DocPosition(); }
};
class pf_Fragments {
  public:
   pf_Frag * getFirst() const;
};
class pt_PieceTable {
   bool getStruxOfTypeFromPosition(PL_ListenerId listenerId, PT_DocPosition docPos, PTStruxType pts, PL_StruxFmtHandle * psfh) const;
   bool _tellAndMaybeAddListener(PL_ListenerId listenerId, bool bAdd);
   pf_Fragments m_fragments;
};
class pf_Frag_Object : public pf_Frag
{
  public:
   virtual bool createSpecialChangeRecord(PX_ChangeRecord ** ppcr, PT_DocPosition dpos, PT_BlockOffset blockOffset) const;
};
bool pt_PieceTable::_tellAndMaybeAddListener(PL_ListenerId listenerId, bool bAdd) 
{
  PL_StruxFmtHandle sfh = 0;
  PT_DocPosition sum = 0;
  UT_uint32 blockOffset = 0;
  for (pf_Frag * pf = m_fragments.getFirst(); (pf); pf=pf->getNext())
  {
      pf_Frag_Object * pfo = static_cast<pf_Frag_Object *> (pf);
      PX_ChangeRecord * pcr = __null;
      bool bStatus1 = false;
      if(sfh != __null)     {
       bStatus1 = pfo->createSpecialChangeRecord(&pcr,sum,blockOffset);
       if (!(bStatus1))
	 return (false);
      }
      else
      {
       PT_DocPosition pos = pf->getPos();
       getStruxOfTypeFromPosition(listenerId,pos,PTX_Block,&sfh);
       bStatus1 = pfo->createSpecialChangeRecord(&pcr,pos,blockOffset);
       if (!(bStatus1))
	 return (false); 
      }
      if (!(bStatus1))
	return (false);
  }
}
