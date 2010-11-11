// { dg-lto-do link }
// { dg-lto-options {{-O3 -Wno-multichar}} } 
// { dg-extra-ld-options "-flto -flto-partition=1to1 -r -nostdlib" }

     extern "C" {
   typedef struct __locale_struct {
  }
   *__locale_t;
   }
       typedef void * POVMSContext;
       struct POVMSData {
   };
        int POVMS_OpenContext (POVMSContext *contextrefptr);
       enum {
    kPOVMsgIdent_InitInfo = 'InIn',  kPOVMsgIdent_RenderOptions = 'ROpt',  kPOVMsgIdent_RenderAll = 'RAll',  kPOVMsgIdent_RenderArea = 'RAre',  kPOVMsgIdent_RenderPause = 'RPau',  kPOVMsgIdent_RenderStop = 'RSto',  kPOVMsgIdent_RenderStarted = 'RRun',  kPOVMsgIdent_RenderDone = 'REnd',  kPOVMsgIdent_FrameStatistics = 'FSta',  kPOVMsgIdent_ParseStatistics = 'PSta',  kPOVMsgIdent_RenderStatistics = 'RSta',  kPOVMsgIdent_Progress = 'Prog',  kPOVMsgIdent_Warning = 'Warn',  kPOVMsgIdent_Error = 'ErrW',  kPOVMsgIdent_FatalError = 'ErrF',  kPOVMsgIdent_Debug = 'Dbug' };
       namespace pov {
   }
       using namespace pov;
       namespace pov {
   int pre_init_flag = 0;
   }
       POVMSContext POVMS_Render_Context = __null;
       void povray_init() {
      if (pre_init_flag == 0)    {
        int err;
        err = POVMS_OpenContext(&POVMS_Render_Context);
     }
   }
