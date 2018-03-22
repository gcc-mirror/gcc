  extern "C" {
    typedef struct _IO_FILE FILE;
    extern struct _IO_FILE *stderr;
    extern int fprintf (FILE *__restrict __stream,       __const char *__restrict __format, ...);
    }
      enum {
        kPOVMSObjectClassID = 'OCLA',     kPOVMSMessageClassID = 'MCLA',     kPOVMSMessageIdentID = 'MIDE',     kPOVMSSourceAddressID = 'MSRC',     kPOVMSDestinationAddressID = 'MDST',     kPOVMSMessageTimeoutID = 'TOUT',     kPOVMSMessageErrorID = 'MERR' };
      typedef void * POVMSContext;
      typedef struct POVMSData POVMSObject, *POVMSObjectPtr;
      typedef struct POVMSData POVMSAttribute, *POVMSAttributePtr;
      struct POVMSData {
        union     {
           struct POVMSNode *root;
       };
    };
      struct POVMSNode {
        struct POVMSNode *next;
        unsigned int key;
        struct POVMSData data;
    };
       int POVMSObject_New (POVMSObjectPtr object, unsigned int objclass);
       int POVMSObject_Set (POVMSObjectPtr object, POVMSAttributePtr attr, unsigned int key);
       int POVMSAttr_Copy (POVMSAttributePtr sourceattr, POVMSAttributePtr destattr);
       int POVMSUtil_GetType (POVMSObjectPtr object, unsigned int key, unsigned int *typevalue);
      namespace pov_base {
    enum {
    kNoError = 0,  kNoErr = kNoError,  kParamErr = -1,  kMemFullErr = -2,  kOutOfMemoryErr = kMemFullErr,  kInvalidDataSizeErr = -3,  kCannotHandleDataErr = -4,  kNullPointerErr = -5,  kChecksumErr = -6,  kParseErr = -7,  kCannotOpenFileErr = -8,  kInvalidDestAddrErr = -9,  kCannotConnectErr = -10,  kDisconnectedErr = -11,  kHostDisconnectedErr = -12,  kNetworkDataErr = -13,  kNetworkConnectionErr = -14,  kObjectAccessErr = -15,  kVersionErr = -16,  kFileDataErr = -17,  kAuthorisationErr = -18,  kDataTypeErr = -19,  kTimeoutErr = -20,  kInvalidContextErr = -21 };
    }
      using namespace pov_base;
      struct POVMSContextData {
    };
      int POVMS_AssertFunction (int cond, const char *str, const char *filename, int line);
       int POVMS_OpenContext(POVMSContext *contextrefptr) {
     POVMSContextData *context = __null;
     if(contextrefptr == __null)   return kParamErr;
     if(POVMS_AssertFunction(context != __null, "POVMS_Open_Context failed, out of memory", "povms.cpp", 283) == false)   return kMemFullErr;
     return kNoErr;
    }
      int POVMS_AssertFunction(int cond, const char *str, const char *filename, int line) {
     if(cond == false)  {
     fprintf(stderr, "POVMS_ASSERT failed in %s line %d: %s\n", filename, (int)line, str);
    }
    }
       int POVMSObject_Copy(POVMSObjectPtr sourceobject, POVMSObjectPtr destobject) {
     POVMSNode *cur = __null;
     POVMSAttribute attr;
     unsigned int t;
     int ret = kNoErr;
     if(sourceobject == destobject)   return kParamErr;
     if(POVMSUtil_GetType(sourceobject, kPOVMSObjectClassID, &t) != kNoErr)   return kObjectAccessErr;
     if(POVMSObject_New(destobject, t) != kNoErr)   return kObjectAccessErr;
     for(cur = sourceobject->root;
    cur != __null;
    cur = cur->next)  {
     if(POVMS_AssertFunction(POVMSAttr_Copy(&(cur->data), &attr) == kNoErr, "POVMSObject_Copy failed, out of memory", "povms.cpp", 2028) == false)   {
   }
     if(POVMS_AssertFunction(POVMSObject_Set(destobject, &attr, cur->key) == kNoErr, "POVMSObject_Copy failed, out of memory", "povms.cpp", 2034) == false)   {
   }
    }
     return ret;
    }
       int POVMSObject_Merge(POVMSObjectPtr sourceobject, POVMSObjectPtr destobject) {
     POVMSNode *cur = __null;
     POVMSAttribute attr;
     unsigned int t1, t2;
     int ret = kNoErr;
     if(destobject == __null)   return kParamErr;
     if(POVMSUtil_GetType(sourceobject, kPOVMSObjectClassID, &t1) != kNoErr)   return kObjectAccessErr;
     if(POVMSUtil_GetType(sourceobject, kPOVMSObjectClassID, &t2) != kNoErr)   return kObjectAccessErr;
     for(cur = sourceobject->root;
    cur != __null;
    cur = cur->next)  {
     if(POVMS_AssertFunction(POVMSObject_Set(destobject, &attr, cur->key) == kNoErr, "POVMSObject_Merge failed, out of memory", "povms.cpp", 2090) == false)   {
   }
    }
     return ret;
    }
