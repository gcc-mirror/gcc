/* PR rtl-optimization/59350 */
/* Testcase by Ryan Mansfield <rmansfield@qnx.com> */

/* { dg-do compile } */
/* { dg-options "-O -g" } */

typedef union
{
  char b[2];
  short NotAnInteger;
}
mDNSOpaque16;
typedef mDNSOpaque16 mDNSIPPort;
typedef struct
{
}
mDNSAddr;
typedef struct DNSQuestion_struct DNSQuestion;
typedef struct mDNS_struct mDNS;
typedef struct __attribute__ ((__packed__))
{
  mDNSOpaque16 id;
  mDNSOpaque16 flags;
}

DNSMessageHeader;
typedef struct __attribute__ ((__packed__))
{
  DNSMessageHeader h;
}

DNSMessage;
struct DNSQuestion_struct
{
  DNSQuestion *next;
  long LastQTime;
  mDNSOpaque16 TargetQID;
};
struct mDNS_struct
{
  long timenow;
  DNSQuestion *Questions;
};
extern long mDNSPlatformOneSecond;
typedef enum
{
  kDNSFlag0_QR_Mask = 0x80, kDNSFlag0_QR_Query = 0x00, kDNSFlag0_QR_Response =
    0x80, kDNSFlag0_OP_Mask = 0x78, kDNSFlag0_OP_StdQuery =
    0x00, kDNSFlag0_OP_Iquery = 0x08, kDNSFlag0_OP_Status =
    0x10, kDNSFlag0_OP_Unused3 = 0x18, kDNSFlag0_OP_Notify =
    0x20, kDNSFlag0_OP_Update = 0x28, kDNSFlag0_QROP_Mask =
    kDNSFlag0_QR_Mask | kDNSFlag0_OP_Mask, kDNSFlag0_AA = 0x04, kDNSFlag0_TC =
    0x02, kDNSFlag0_RD = 0x01, kDNSFlag1_RA = 0x80, kDNSFlag1_Zero =
    0x40, kDNSFlag1_AD = 0x20, kDNSFlag1_CD = 0x10, kDNSFlag1_RC_Mask =
    0x0F, kDNSFlag1_RC_NoErr = 0x00, kDNSFlag1_RC_FormErr =
    0x01, kDNSFlag1_RC_ServFail = 0x02, kDNSFlag1_RC_NXDomain =
    0x03, kDNSFlag1_RC_NotImpl = 0x04, kDNSFlag1_RC_Refused =
    0x05, kDNSFlag1_RC_YXDomain = 0x06, kDNSFlag1_RC_YXRRSet =
    0x07, kDNSFlag1_RC_NXRRSet = 0x08, kDNSFlag1_RC_NotAuth =
    0x09, kDNSFlag1_RC_NotZone = 0x0A
}
TSIG_ErrorCode;
void
uDNS_ReceiveMsg (mDNS * const m, DNSMessage * const msg,
		 const char * const end, const mDNSAddr * const srcaddr,
		 const mDNSIPPort srcport)
{
  DNSQuestion *qptr;
  char StdR = kDNSFlag0_QR_Response | kDNSFlag0_OP_StdQuery;
  char QR_OP = (char) (msg->h.flags.b[0] & kDNSFlag0_QROP_Mask);
  if (QR_OP == StdR)
    {
     if (uDNS_ReceiveTestQuestionResponse (m, msg, end, srcaddr, srcport))
	return;
      for (qptr = m->Questions; qptr; qptr = qptr->next)
	if (msg->h.flags.b[0] & kDNSFlag0_TC
	    && ((qptr->TargetQID).NotAnInteger == (msg->h.id).NotAnInteger)
	    && m->timenow - qptr->LastQTime < (60 * mDNSPlatformOneSecond))
	  {
	  }
    }
}
