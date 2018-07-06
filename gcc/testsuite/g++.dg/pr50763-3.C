/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -ftree-tail-merge" } */

class v2d {
public:
   double x;
   double y;
};

class v3d {
public:
   double x;
   v3d() {}
   v3d(const v2d & cr2Dv) {}
};

class e2d {
protected:
   v2d _Min;
   v2d _Max;
public:
   int cop2d(const v2d & rPnt) const;
   v2d clp2d(const v2d & rPnt) const;
};

inline int e2d::cop2d(const v2d & rPnt) const {
   int bRet = 1;
   if (rPnt.x < _Min.x) bRet = 0;
   else if (rPnt.x > _Max.x) bRet = 0;
   else if (rPnt.y > _Max.y) bRet = 0;
   return bRet;
}

inline v2d e2d::clp2d(const v2d & rPnt) const {
   v2d sRet = rPnt;
   if (rPnt.x < _Min.x) sRet.x = _Min.x;
   if (rPnt.y < _Min.y) sRet.y = _Min.y;
   if (rPnt.x > _Max.x) sRet.x = _Max.x;
   if (rPnt.y > _Max.y) sRet.y = _Max.y;
   return sRet;
}

class sExt {
protected:
   e2d _Dom;
   long eval() const;
   long evalPoint(const v2d & crUV, v3d & rPnt) const;
};

long sExt::evalPoint(const v2d & crUV, v3d & rPnt) const {
   v3d sUV = crUV;
   if (!_Dom.cop2d(crUV)) {
      sUV = _Dom.clp2d(crUV);
   }
   return eval();
}   
