typedef unsigned uint32_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef struct {
 uint8_t mbxOwner:1;
 uint8_t mbxHc:1;
 uint8_t mbxReserved:6;
 uint8_t mbxCommand : 8;
 uint16_t mbxStatus : 16;
} MAILBOX_t;
uint32_t f(void) {
       uint32_t mbox;
 mbox = 0;
 ((MAILBOX_t *)&mbox)->mbxCommand = 0x24;
 ((MAILBOX_t *)&mbox)->mbxOwner = 1;
return mbox;
}
