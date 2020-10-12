struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef struct _message
{
  FILE *fp;
} MESSAGE;
extern FILE *fopen (const char *__restrict __filename,
                    const char *__restrict __modes);
FILE *f (void);
int imap_fetch_message (int i, MESSAGE *msg, char *p)
{
  if ((msg->fp = i ? 0 : f ()))
    return 0;
  if (p)
    msg->fp = fopen (p, "r");
  return -1;
}
