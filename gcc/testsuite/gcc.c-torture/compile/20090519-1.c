typedef struct { int licensed;  } __pmPDUInfo;
void __pmDecodeXtendError (int *);
void do_handshake(void)
{
  __pmPDUInfo *pduinfo;
  int challenge;
  __pmDecodeXtendError(&challenge);
  pduinfo = (__pmPDUInfo *)&challenge;
  *pduinfo = *pduinfo;
}

