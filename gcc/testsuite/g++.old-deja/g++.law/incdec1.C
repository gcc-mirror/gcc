// { dg-do assemble  }
// { dg-options "-w -O3" }
// GROUPS passed incdec
typedef void* Pix;
class ndbArtPtrVec 
{
public:
                        ndbArtPtrVec (ndbArtPtrVec&);
};
class intndbArtPtrVecMap
{
protected:
  int                   count;
  ndbArtPtrVec                   def;
                        intndbArtPtrVecMap(ndbArtPtrVec& dflt);
  virtual ndbArtPtrVec&          operator [] (int  key) = 0;  
  virtual void          del(int  key) = 0;        
public:
  virtual Pix           first() = 0;              
  virtual void          next(Pix& i) = 0;         
  virtual int&          key(Pix i) = 0;           
  virtual ndbArtPtrVec&          contents(Pix i) = 0;      
};
struct intndbArtPtrVecAVLNode
{
  ndbArtPtrVec                 cont;
                      intndbArtPtrVecAVLNode(int  h, ndbArtPtrVec& c, 
                                    intndbArtPtrVecAVLNode* l=0, intndbArtPtrVecAVLNode* r=0);
};
class intndbArtPtrVecAVLMap : intndbArtPtrVecMap 
{
protected:
  intndbArtPtrVecAVLNode*   root;
public:
  ndbArtPtrVec&          operator [] (int  key);
  intndbArtPtrVecAVLMap(intndbArtPtrVecAVLMap&);
};
ndbArtPtrVec& intndbArtPtrVecAVLMap::operator [] (int  item)
{
    ++count;
    root = new intndbArtPtrVecAVLNode(item, def);
}
intndbArtPtrVecAVLMap::intndbArtPtrVecAVLMap(intndbArtPtrVecAVLMap& b) :intndbArtPtrVecMap(b.def)
{
  for (Pix i = b.first(); i != 0; b.next(i)) 
    (*this)[b.key(i)] = b.contents(i);
}
