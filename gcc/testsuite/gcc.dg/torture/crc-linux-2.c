/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stddef.h>
typedef unsigned char  u8;
typedef unsigned short u16;
typedef unsigned char __u8;
typedef unsigned short __u16;
struct i2c_msg {
    __u16 addr;
    __u16 flags;
#define I2C_M_RD                0x0001  /* guaranteed to be 0x0001! */
    /* ... */
    __u16 len;
    __u8 *buf;
};

#define POLY    (0x1070U << 3)
static u8 crc8(u16 data)
{
  int i;

  for (i = 0; i < 8; i++) {
      if (data & 0x8000)
	data = data ^ POLY;
      data = data << 1;
    }
  return (u8)(data >> 8);
}

/**
* i2c_smbus_pec - Incremental CRC8 over the given input data array
* @crc: previous return crc8 value
* @p: pointer to data buffer.
* @count: number of bytes in data buffer.
*
* Incremental CRC8 over count bytes in the array pointed to by p
*/
u8 i2c_smbus_pec(u8 crc, u8 *p, size_t count)
{
  int i;

  for (i = 0; i < count; i++)
    crc = crc8((crc ^ p[i]) << 8);
  return crc;
}
static inline u8 i2c_8bit_addr_from_msg(const struct i2c_msg *msg)
{
  return (msg->addr << 1) | (msg->flags & I2C_M_RD ? 1 : 0);
}


/* Assume a 7-bit address, which is reasonable for SMBus */
u8 i2c_smbus_msg_pec(u8 pec, struct i2c_msg *msg)
{
  /* The address will be sent first */
  u8 addr = i2c_8bit_addr_from_msg(msg);
  pec = i2c_smbus_pec(pec, &addr, 1);

  /* The data buffer follows */
  return i2c_smbus_pec(pec, msg->buf, msg->len);
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
