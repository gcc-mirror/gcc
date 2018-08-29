// { dg-lto-do link }
// { dg-lto-options {{-O3 -Wno-multichar -Wno-return-type}} } 
// { dg-extra-ld-options "-flto -flto-partition=1to1 -r -nostdlib" }

    extern "C" {
  typedef struct {
    union   {
   }
  __value;
  }
  __mbstate_t;
  struct _IO_marker {
  };
  extern "C" {
    }
  };
    namespace pov_base {
  class IOBase {
  };
  }
    namespace pov {
  typedef double VECTOR[3];
  enum {
   X = 0,  Y = 1,  Z = 2,  T = 3 };
  inline void Assign_Vector(VECTOR d, VECTOR s) {
  }
  typedef float BBOX_VAL;
  typedef BBOX_VAL BBOX_VECT[3];
  typedef struct Bounding_Box_Struct BBOX;
  struct Bounding_Box_Struct {
  };
  inline void Make_BBox_from_min_max(BBOX& BBox, BBOX_VECT mins, BBOX_VECT maxs) {
  }
  typedef long long COUNTER;
  inline double DBL_Counter(COUNTER x) {
  }
  struct Image_Struct {
    union   {
   }
  data;
  };
  struct Density_file_Data_Struct {
  union {
 }
  Vals;
  };
  struct Pigment_Struct {
  union {
 struct {
 }
 Brick;
 struct {
 }
 Fractal;
 struct {
 }
 Function;
 }
  Vals;
  };
  typedef enum shelltype {
     PRE_SCENE_SHL = 0,    PRE_FRAME_SHL,    POST_FRAME_SHL,    POST_SCENE_SHL,    USER_ABORT_SHL,    FATAL_SHL,    MAX_SHL }
  SHELLRET;
  }
    typedef void * POVMSContext;
    struct POVMSData {
  };
     int POVMS_OpenContext (POVMSContext *contextrefptr);
    namespace pov_base {
  enum {
   kFalseErr = 1,  kOutOfSyncErr = 2,  kNotNowErr = kOutOfSyncErr,  kQueueFullErr = 3 };
  }
    namespace pov_base {
  class OTextStream {
  };
  }
    enum {
   kPOVMsgClass_RenderControl = 'Ctrl',  kPOVMsgClass_RenderOutput = 'Outp',  kPOVMsgClass_IniOptions = 'IniO',  kPOVMsgClass_Miscellaneous = 'Misc' };
    namespace pov_base {
  class PlatformBase {
  };
  }
    class POVMS_Container {
    template<class T> void Read(T& stream)   {
    }
  };
    class POVMS_MessageReceiver {
   private:   class HandlerOO   {
    };
   protected:   template<class T> class MemberHandlerOO : public HandlerOO   {
    };
    class FunctionHandlerOO : public HandlerOO   {
    };
    template<class T> void InstallFront(unsigned int hclass, unsigned int hid, T *cptr, typename MemberHandlerOO<T>::MemberHandlerPtr hptr)   {
  }
  };
    namespace pov_base {
  class TextStreamBuffer {
  };
  }
    namespace pov_frontend {
  using namespace pov_base;
  class MessageOutput : public POVMS_MessageReceiver {
  };
  class DefaultPlatformBase : public PlatformBase {
  };
  }
    using namespace pov;
    namespace pov {
  int pre_init_flag = 0;
  }
    POVMSContext POVMS_Render_Context = __null;
    int main(int argc, char **argv) {
  }
    void povray_init() {
     if (pre_init_flag == 0)    {
        int err;
        err = POVMS_OpenContext(&POVMS_Render_Context);
     }
  }
