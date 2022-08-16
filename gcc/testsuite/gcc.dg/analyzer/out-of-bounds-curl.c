/* { dg-additional-options "-O2" } */
#include <string.h>

/* Reduced from curl lib/smb.c.  */
typedef int CURLcode;

struct smb_conn {
  // [...]
  char *user;
};

struct smb_setup {
  // [...]
  char bytes[48];
} __attribute__((packed));

struct connectdata {
  // [...]
  struct smb_conn *smbc;
};

CURLcode smb_send_setup (struct connectdata *conn)
{
  struct smb_conn *smbc = conn->smbc;
  struct smb_setup msg;
  char *p = msg.bytes;
  unsigned char lm[24];

  /* Init to prevent uninit warning.  */
  memset(&msg, 0, sizeof(msg));
  memset (&lm, 0, sizeof(lm));

  memcpy(p, lm, sizeof(lm));
  p += sizeof(lm);
  /* Had a false-positive overflow at p. Checker had a number of bytes copied
     relative to the start but offset points in the middle the field.  */
  strcpy(p, (smbc->user));
  p += strlen(smbc->user) + 1;

  return 1;
}
