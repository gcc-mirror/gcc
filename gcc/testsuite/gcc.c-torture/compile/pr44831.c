typedef unsigned char UCHAR, *PUCHAR;
typedef void *HANDLE;
typedef struct _NCB {
    UCHAR ncb_reserve[10];
} NCB, *PNCB;
struct NBCmdQueue {
    PNCB head;
};
PNCB *NBCmdQueueFindNBC(struct NBCmdQueue *queue, PNCB ncb)
{
  PNCB *ret = &queue->head;
  while (ret && *ret != ncb)
    ret = (PNCB *)((*ret)->ncb_reserve + sizeof(HANDLE));
}

