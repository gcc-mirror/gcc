// Build don't link:
// Origin: Loring Holden <lsh@cs.brown.edu>

template <class T>
class REFptr {
   public:
      REFptr();
      REFptr(T *pObj);
      virtual ~REFptr();
      operator T* () const;
};

class GEL;
class GELsubc {
   public :
      virtual GEL *GELcast() const;
};
class GELptr : public REFptr<GEL>{
   public :                                        
      GELptr(const GELptr  &p);
      GELptr(const GELsubc &p);
};
class GEL { };

class GEOM;
class GEOMptr : public REFptr<GEOM>, public GELsubc {
   public:
      GEOMptr() { }
      GEOMptr(GEOM  *g); 
};
class GEOM : public GEL {
   public: 
      GEOM(const GEOMptr &o);
      GEOM();
};

class TEXT2D;
class TEXT2Dptr : public REFptr<TEXT2D> {
   public:
      TEXT2Dptr();
      TEXT2Dptr(TEXT2D *g); 
};
class TEXT2D : public GEOM { };

void testit(const GELptr g);

void
FPS()
{
  TEXT2Dptr fps_text;
  testit(GEOMptr(&*fps_text));
}
